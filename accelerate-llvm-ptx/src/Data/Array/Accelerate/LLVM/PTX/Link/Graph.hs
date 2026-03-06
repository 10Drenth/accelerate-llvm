{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Array.Accelerate.LLVM.PTX.Link.Graph (linkProgram, runGraphProgram, GraphProgram) where

import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.LLVM.PTX.Kernel (PTXKernel)
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.Type (ScalarType)
import Data.Array.Accelerate.Error (internalError)
import qualified Data.Map as M
import Data.Array.Accelerate.AST.Idx
import Control.Concurrent (readMVar, putMVar, takeMVar, forkIO)
import Data.IORef (readIORef, IORef, writeIORef)
import Data.Array.Accelerate.Representation.Elt (showElt, bytesElt)
import Data.Array.Accelerate.Array.Buffer (bufferToList, Buffer (Buffer), memoryByteSize, MutableBuffer (..), newBuffer, writeBuffer, indexBuffer)
import Foreign
import Data.Maybe (mapMaybe)
import Data.Bifunctor
import GHC.Conc (PrimMVar, newStablePtrPrimMVar)
import Control.Concurrent.MVar (newEmptyMVar, MVar)


data GraphProgram = GraphProgram Nodes Allocations

newtype NIndex = NodeIndex Int
  deriving (Eq, Ord)
newtype AIndex = AIndex Int
  deriving (Eq, Ord)
aIdxToInt :: AIndex -> Int
aIdxToInt (AIndex v) = v

newtype EventIndex = EventIndex Int
  deriving (Eq, Ord, Show)

type Nodes = M.Map NIndex NContent
type Allocations = M.Map AIndex [AIndex]

data EventContents = EventContents [NIndex] -- Incoming
                                   [NIndex] -- Outgoing
type Events = M.Map EventIndex EventContents

data LHSNode t where
  LHSNodeStart :: NIndex -> LHSNode Signal
  LHSNodeEnd :: NIndex -> LHSNode SignalResolver
  LHSNodeType :: AIndex -> ScalarType e -> LHSNode t
  LHSNodeSignal :: EventIndex -> LHSNode Signal
  LHSNodeResolver :: EventIndex -> LHSNode SignalResolver
instance Show (LHSNode t) where
  show (LHSNodeStart idx) = "StartNode: " ++ show idx
  show (LHSNodeEnd idx) = "EndNode: " ++ show idx
  show (LHSNodeType idx _) = "TypeNode" ++ show idx
  show (LHSNodeSignal idx) = "SignalNode: " ++ show idx
  show (LHSNodeResolver idx) = "ResolverNode" ++ show idx

data NContent = CopyNode [NIndex] -- Dependencies
                            AIndex -- From
                            AIndex -- To
                 | Input AIndex
                 | Output AIndex [NIndex] -- Dependencies
                 | EmptyNode [NIndex] -- Dependencies
                 | AllocNode Int -- Element bytesize
                             [AIndex] -- dims
                             [NIndex] -- Dependencies 
  deriving (Show)

addDeps :: NContent -> [NIndex] -> NContent
addDeps (EmptyNode deps) = EmptyNode . (deps ++ )
addDeps (Output a deps) = Output a . (deps ++ )
addDeps (CopyNode deps f t) = \d -> CopyNode (deps ++ d) f t
addDeps (Input a) = const $ Input a
addDeps (AllocNode bs dims deps) = AllocNode bs dims . (deps ++)


getDeps :: NContent -> [NIndex]
getDeps (EmptyNode deps) = deps
getDeps (Output _ deps) = deps
getDeps (CopyNode deps _ _) = deps
getDeps (Input _) = []
getDeps (AllocNode _ _ deps) = deps

linkProgram :: UniformScheduleFun PTXKernel () f -> GraphProgram
linkProgram = convertFun

convertFun :: forall t. UniformScheduleFun PTXKernel () t -> GraphProgram
convertFun (Sbody _) = error ""
convertFun (Slam lhs1 (Slam lhs2 f)) = convertFun (Slam (LeftHandSidePair lhs1 lhs2) f)
convertFun (Slam lhs (Sbody body)) = let
  (lhsNodes, m, a) = convertLHS undefined (lhsToTupR lhs) M.empty M.empty
  lhsenv = push' Empty (lhs, lhsNodes)
  (m', a', _) = convertBody lhsenv body [] m a M.empty
  in GraphProgram m' a'

convertBody :: Env LHSNode env -> UniformSchedule PTXKernel env -> [NIndex] -> Nodes -> Allocations -> Events -> (Nodes, Allocations, Events)
convertBody _ Return _ m a e = (m, a, e)
convertBody env (Spawn l r) deps m a e = let
  (m', a', e') = convertBody env l deps m a e
  in convertBody env r deps m' a' e'
convertBody env (Effect (SignalAwait signals) Return) deps m a e = let
  (deps', e') = getDependenciesFromEnv env deps signals e
  m' = M.insert (NodeIndex $ M.size m) (EmptyNode (deps' ++ deps)) m
  in (m', a, e')
convertBody env (Effect (SignalAwait signals) next) deps m a e = let
  (deps', e') = getDependenciesFromEnv env deps signals e
  in convertBody env next (deps' ++ deps) m a e'
convertBody env (Effect (SignalResolve signals) next) deps m a e = let
  (forwarddeps, e') = getForwardDependenciesFromEnv env deps signals e
  m' = foldr (M.adjust (`addDeps` deps)) m forwarddeps
  in convertBody env next deps m' a e'
convertBody env (Alet lhs (NewSignal _) next) deps m a e = let
  eventIdx = EventIndex $ M.size e
  e' = M.insert eventIdx (EventContents [] []) e
  env' =  push' env (lhs, (LHSNodeSignal eventIdx, LHSNodeResolver eventIdx))
  in convertBody env' next deps m a e'

convertBody env (Alet lhs (RefRead rref) (Effect (RefWrite wref _) next)) deps m a e = let
  idxIn :: AIndex
  idxIn = case prj' (varIdx rref) env of (LHSNodeType idx _) -> idx
  env' = push' env (lhs, undefined) -- It will only be read here

  idxOut :: AIndex
  idxOut = case prj' (varIdx wref) env' of (LHSNodeType idx _) -> idx

  a' = M.adjust (idxIn :) idxOut a
  nIdx :: NIndex
  nIdx = NodeIndex $ M.size m
  m' = M.insert nIdx (CopyNode deps idxIn idxOut) m

  in convertBody env' next [nIdx] m' a' e
convertBody _ _ _ _ _ _ = internalError "Unexpected body contents in schedule. Currently on handles Identity functions"

getDependenciesFromEnv :: Env LHSNode env -> [NIndex] -> [Idx env Signal] -> Events -> ([NIndex], Events)
getDependenciesFromEnv env deps ss e = (concatMap (\idx -> f $ prj' idx env) ss, updatedEvents)
  where
    f :: LHSNode Signal -> [NIndex]
    f (LHSNodeStart i) = [i]
    f (LHSNodeSignal i) = case e M.! i of EventContents ns _ -> ns
    f _ = error "unreachable"

    updatedEvents :: Events
    updatedEvents = foldr (f' . (`prj'` env)) e ss

    f' :: LHSNode Signal -> Events -> Events
    f' (LHSNodeSignal i) m = M.adjust (\(EventContents i' o') -> EventContents (deps ++ i') o') i m
    f' _ m = m

getForwardDependenciesFromEnv :: Env LHSNode env -> [NIndex] -> [Idx env SignalResolver] -> Events -> ([NIndex], Events)
getForwardDependenciesFromEnv env deps ss e = (concatMap (\idx -> f $ prj' idx env) ss, updatedEvents)
  where


    f :: LHSNode SignalResolver -> [NIndex]
    f (LHSNodeEnd i) = [i]
    f (LHSNodeResolver i) = case e M.! i of EventContents _ ns -> ns
    f _ = error "unreachable"

    updatedEvents :: Events
    updatedEvents = foldr (f' . (`prj'` env)) e ss

    f' :: LHSNode SignalResolver -> Events -> Events
    f' (LHSNodeResolver i) m = M.adjust (\(EventContents i' o') -> EventContents (deps ++ i') o') i m
    f' _ m = m

convertLHS :: t -> BasesR t -> Nodes -> Allocations -> (Distribute LHSNode t, Nodes, Allocations)
-- Unit
convertLHS _ TupRunit m a = ((), m, a)
-- Scalar input argument
convertLHS _ (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref (GroundRscalar tp))) m a =
  let alloc = AIndex $ M.size a in
  ( ( LHSNodeStart (NodeIndex (M.size m))
    , LHSNodeType (AIndex (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Input alloc) m
  , M.insert alloc [] a
  )
-- Buffer input argument
convertLHS _ (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref (GroundRbuffer tp))) m a =
  let alloc = AIndex $ M.size a in
  ( ( LHSNodeStart (NodeIndex (M.size m))
    , LHSNodeType (AIndex (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Input alloc) m
  , M.insert alloc [] a
  )
-- Scalar output argument
convertLHS _ (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRscalar tp))) m a =
  let alloc = AIndex $ M.size a in
  ( ( LHSNodeEnd (NodeIndex (M.size m))
    , LHSNodeType (AIndex (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Output alloc []) m
  , M.insert alloc [] a
  )
-- Buffer output argument
convertLHS  _ (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRbuffer tp))) m a =
  let alloc = AIndex $ M.size a in
  ( ( LHSNodeEnd (NodeIndex (M.size m))
    , LHSNodeType (AIndex (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Output alloc []) m
  , M.insert alloc [] a
  )
-- Pair
convertLHS _ (TupRpair t1 t2) m a = let
  (res1, m', a') = convertLHS undefined t1 m a
  (res2, m'', a'') = convertLHS undefined t2 m' a'
  in ((res1, res2), m'', a'')
convertLHS _ _ _ _ = internalError "Unexpected types in the input or output of an Acc function"

instance Show GraphProgram where
  show (GraphProgram ns as) = "\n\n===Node Graph==\n"
                            ++ prettyPrintMap ns
                            ++ "\n\n===Data Graph===\n"
                            ++ prettyPrintMap as
                            ++ "\n\n"

prettyPrintMap :: (Ord k, Show k, Show v) => M.Map k v -> String
prettyPrintMap m = unlines $ map (\k -> show k ++ ": " ++ show (m M.! k)) $ M.keys m

instance Show NIndex where
  show (NodeIndex i) = "n" ++ show i
instance Show AIndex where
  -- show (SomeAllocationIndex _ i) = "d" ++ show i
  show (AIndex i) = "d" ++ show i

data HostAllocation where
  ScalarAllocation :: Buffer t -> HostAllocation
  BufferAllocation :: Buffer t -> HostAllocation

type InputValues = M.Map AIndex HostAllocation
type OutputAllocations = M.Map AIndex HostAllocation
type OutWrites = M.Map AIndex (MVar () -> OutputAllocations -> IO ())


allocateHostOutBuffers :: M.Map AIndex Int -> IO OutputAllocations
allocateHostOutBuffers = mapM f
  where
    f :: Int -> IO HostAllocation
    f byteSize = do
      ptr <- mallocForeignPtrBytes byteSize
      return $ BufferAllocation (Buffer ptr)

data Pass1 = Pass1
           { currentIndex :: AIndex
           , inputSizes :: M.Map AIndex Int
           , inputValues :: InputValues
           , outputWriteOps :: OutWrites
           }

incrementIndex :: Pass1 -> Pass1
incrementIndex p@(Pass1 {currentIndex}) = case currentIndex of
  (AIndex i) -> p{currentIndex = AIndex (i + 1)}

foreign import ccall unsafe "run_graph" run_graph_c
  :: Word32 -- Nodecount
  -> Ptr Word32 -- Node dependency counts
  -> Ptr (Ptr Word32) -- Node dependencies
  -> Ptr Word32 -- Node contents
  -> Word32 -- Alloc count
  -> Ptr Word32 -- Alloc sizes
  -> Ptr (Ptr Word8) -- Input data
  -> Ptr (Ptr Word8) -- Output data
  -> Ptr (StablePtr PrimMVar) -- Output mvars
  -> StablePtr PrimMVar -- Done Mvar
  -> IO ()

runGraphProgram :: GraphProgram -> TupR BaseR t -> t -> IO ()
runGraphProgram (GraphProgram n a) tup v = do

  p1 <- inspectInputAllocSizes tup v $ Pass1
    { currentIndex = AIndex 0
    , inputSizes = M.empty
    , inputValues = M.empty
    , outputWriteOps = M.empty
    }

  putStrLn $ "Input sizes: \n" ++ prettyPrintMap (inputSizes p1)
  let bytesizes = propagateAllocSizes a (inputSizes p1)
  putStrLn $ "Propagated sizes: \n" ++ prettyPrintMap bytesizes

  outputBuffers <- allocateHostOutBuffers bytesizes

  doneMVar <- newEmptyMVar
  doneMVarPtr <- newStablePtrPrimMVar doneMVar

  let
    node_count = M.size n
    n_nodes_c = (fromIntegral node_count :: Word32)
    bytesizes' = [fromIntegral (bytesizes M.! AIndex i) | i <- [0..(alloc_count-1)]]
    alloc_count = M.size a
    alloc_count_c = (fromIntegral alloc_count :: Word32)
    (node_dependency_counts, node_dependencies) = getNodeDependencies n
    n_outputs = M.size (outputWriteOps p1)

  node_dependency_counts_fp <- mallocForeignPtrArray node_count
  node_contents_fp <- mallocForeignPtrArray $ node_count * 4
  bytesizes_fp <- mallocForeignPtrArray alloc_count
  input_data_fp <- mallocForeignPtrArray (M.size $ inputSizes p1)
  output_data_fp <- mallocForeignPtrArray alloc_count
  output_mvars_fp <- mallocForeignPtrArray alloc_count


  withDeps node_dependencies $ \deps -> do
    node_deps_fp <- mallocForeignPtrArray node_count

    withForeignPtr node_deps_fp $ \nodes_deps_c -> do
      pokeArray nodes_deps_c deps

      withForeignPtr node_dependency_counts_fp $ \node_dependency_counts_c -> do
        pokeArray node_dependency_counts_c node_dependency_counts

        withForeignPtr node_contents_fp $ \node_contents_c -> do
          mapM_ (\nid -> writeNodeContents nid (n M.! NodeIndex nid) node_contents_c) [0..(node_count-1)]

          withForeignPtr bytesizes_fp $ \bytesizes_c -> do
            pokeArray bytesizes_c bytesizes'

            withFps (map getHostAllocFp (ascElems (inputValues p1))) $ \input_data_ptrs -> do
              withForeignPtr input_data_fp $ \input_data_c -> do
                pokeArray input_data_c input_data_ptrs
                withFps (map getHostAllocFp (ascElems outputBuffers)) $ \output_data_ptrs -> do
                  withForeignPtr output_mvars_fp $ \output_mvars_c -> do
                    mvars <- mapM (\aid -> do
                        mvar <- newEmptyMVar
                        case outputWriteOps p1 M.!? aid of
                          (Just op) -> op mvar outputBuffers
                          Nothing -> return ()
                        return mvar
                        ) (M.keys a)
                    mvarPtrs <- mapM newStablePtrPrimMVar mvars
                    pokeArray output_mvars_c mvarPtrs

                    withForeignPtr output_data_fp $ \output_data_c -> do
                      pokeArray output_data_c output_data_ptrs


                      putStrLn "starting C runtime from haskell.."
                      run_graph_c n_nodes_c node_dependency_counts_c nodes_deps_c node_contents_c alloc_count_c bytesizes_c input_data_c output_data_c output_mvars_c doneMVarPtr
                      putStrLn "waiting.."
                      _ <- takeMVar doneMVar
                      putStrLn "Done!"
                      mapM_ (`putMVar` ()) mvars
                      putStrLn "Done resolving outputs!"



  return ()

ascElems :: Ord a => M.Map a b -> [b]
ascElems = map snd . M.toAscList

getHostAllocFp :: HostAllocation -> ForeignPtr Word8
getHostAllocFp (ScalarAllocation (Buffer fptr)) = castForeignPtr fptr
getHostAllocFp (BufferAllocation (Buffer fptr)) = castForeignPtr fptr


writeNodeContents :: Int -> NContent -> Ptr Word32 -> IO ()
writeNodeContents idx nodeContent ptr = case nodeContent of
  (CopyNode _ a1 a2) -> do
    pokeType 0
    pokeA1 a1
    pokeA2 a2
  (Input a) -> do
    pokeType 1
    pokeA1 a
  (Output a _) -> do
    pokeType 2
    pokeA1 a
  (EmptyNode _) -> do
    pokeType 3
  (AllocNode {}) -> undefined
  where
    cursor0 = idx * 4
    cursor1 = cursor0 + 1
    cursor2 = cursor0 + 2
    pokeType = pokeElemOff ptr cursor0
    pokeA1 = pokeElemOff ptr cursor1 . fromIntegral . aIdxToInt
    pokeA2 = pokeElemOff ptr cursor2 . fromIntegral . aIdxToInt

withDeps :: Storable a => [[a]] -> ([Ptr a] -> IO b) -> IO b
withDeps xs op = do
  fps <- traverse (mallocForeignPtrArray . length) xs
  xs' <- mapM (\(fp, es) -> withForeignPtr fp (\ptr -> do pokeArray ptr es; return ptr)) (zip fps xs)
  op xs'

withFps :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withFps fps op = do
  xs' <- mapM (`withForeignPtr` return) fps
  op xs'

getNodeDependencies :: Nodes -> ([Word32] ,[[Word32]])
getNodeDependencies n = let
  deps = map (\(_, v) -> map (\(NodeIndex i) -> fromIntegral i) $ getDeps v) $ M.toAscList n
  n_deps = map (fromIntegral . length) deps
  in (n_deps, deps)

inspectInputAllocSizes :: TupR BaseR t -> t -> Pass1 -> IO Pass1
-- Unit
inspectInputAllocSizes TupRunit _ p = return p
-- Scalar input argument. ASSUMES INPUT MVARS HAVE BEEN RESOLVED
inspectInputAllocSizes (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref (GroundRscalar tp))) (Signal mvar, Ref input) p = do
  readMVar mvar
  val <- readIORef input

  let byteSize = max 1 (bytesElt (TupRsingle tp))
  mbuffer@(MutableBuffer buffer) <- newBuffer tp 1
  writeBuffer tp mbuffer 0 val
  let vs' = M.insert (currentIndex p) (ScalarAllocation (Buffer buffer)) (inputValues p)

  putStrLn "Input scalar value:"
  putStrLn $ "type: " ++ show tp ++ ", bytesize: " ++ show byteSize
  print (showElt (TupRsingle tp) val)

  let m' = M.insert (currentIndex p) byteSize (inputSizes p)

  return (incrementIndex (Pass1 {currentIndex = currentIndex p, inputSizes = m', inputValues = vs', outputWriteOps = outputWriteOps p}))
-- Buffer input argument. ASSUMES INPUT MVARS HAVE BEEN RESOLVED
inspectInputAllocSizes (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref (GroundRbuffer tp))) (Signal mvar, Ref input)  p = do
  readMVar mvar
  val@(Buffer hostPtr) <- readIORef input
  byteSize <- withForeignPtr hostPtr (memoryByteSize . castPtr)
  let byteSize' = max 1 $ fromIntegral byteSize
  let vs' = M.insert (currentIndex p) (BufferAllocation val) (inputValues p)

  putStrLn "Input buffer value:"
  putStrLn $ "type: " ++ show tp ++ ", bytesize: " ++ show byteSize'
  let l = bufferToList tp (byteSize' `div` bytesElt (TupRsingle tp)) val
  let s = concatMap (\e -> ' ' : showElt (TupRsingle tp) e) l
  print $ "[" ++ s ++ " ]"

  let m' = M.insert (currentIndex p) byteSize' (inputSizes p)

  return (incrementIndex (Pass1 {currentIndex = currentIndex p, inputSizes = m', inputValues = vs', outputWriteOps = outputWriteOps p}))
-- Scalar output argument
inspectInputAllocSizes (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRscalar tp))) (SignalResolver mvar, OutputRef output) p = do
  let
    f :: MVar () -> OutputAllocations -> IO ()
    f mvar' outBufs = do
      _ <- forkIO $ do
        putStrLn "HASKELL Starting the wait for scalar MVAR!"
        _ <- takeMVar mvar'
        putStrLn "HASKELL scalar MVAR Done!"
        writeOutputScalar (currentIndex p) tp output outBufs
        putMVar mvar ()
      return ()

  let writes' = M.insert (currentIndex p) f (outputWriteOps p)
  return (incrementIndex (Pass1 {currentIndex = currentIndex p, inputSizes = inputSizes p, inputValues = inputValues p, outputWriteOps = writes'}))
-- Buffer output argument
inspectInputAllocSizes (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRbuffer _))) (SignalResolver mvar, OutputRef output) p = do
  let
    f :: MVar () -> OutputAllocations -> IO ()
    f mvar' outBufs = do
      _ <- forkIO $ do
        putStrLn "HASKELL Starting the wait for buffer MVAR!"
        _ <- takeMVar mvar'
        putStrLn "HASKELL buffer MVAR Done!"
        writeOutputBuffer (currentIndex p) output outBufs
        putMVar mvar ()
      return ()

  let writes' = M.insert (currentIndex p) f (outputWriteOps p)
  return (incrementIndex (Pass1 {currentIndex = currentIndex p, inputSizes = inputSizes p, inputValues = inputValues p, outputWriteOps = writes'}))
-- Pair
inspectInputAllocSizes (TupRpair t1 t2) (v1, v2) p = do
  p' <- inspectInputAllocSizes t1 v1 p
  inspectInputAllocSizes t2 v2 p'
inspectInputAllocSizes _ _  _ = internalError "Unexpected types in the input or output of an Acc function"

writeOutputScalar :: AIndex -> ScalarType e -> IORef e -> M.Map AIndex HostAllocation -> IO ()
writeOutputScalar idx tp ref m = let buf = getValue idx m in writeIORef ref (indexBuffer tp buf 0)

writeOutputBuffer :: AIndex -> IORef (Buffer e) -> M.Map AIndex HostAllocation -> IO ()
writeOutputBuffer idx ref m = let buf = getValue idx m in writeIORef ref buf

getValue :: AIndex -> M.Map AIndex HostAllocation -> Buffer e
getValue idx m = case m M.! idx of BufferAllocation (Buffer p) -> Buffer (castForeignPtr p)
                                   ScalarAllocation (Buffer p) -> Buffer (castForeignPtr p)



propagateAllocSizes :: Allocations -> M.Map AIndex Int -> M.Map AIndex Int
propagateAllocSizes deps sizes | all (`M.member` sizes) (M.keys deps) = sizes
                               | otherwise = let
                                  remainingKeys = filter (\k -> not (M.member k sizes)) (M.keys deps)
                                  withSizes = zip remainingKeys (map (\k -> mapMaybe (sizes M.!?) (deps M.! k)) remainingKeys)
                                  nonEmpty = map (second head) $ filter (not . null . snd) withSizes
                                  in M.union sizes (M.fromList nonEmpty)