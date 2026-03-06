{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs #-}
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
import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Graph.Build as Graph
import GHC.Conc (PrimMVar, newStablePtrPrimMVar)
import Control.Concurrent.MVar (newEmptyMVar, MVar)


data GraphProgram = GraphProgram Nodes Allocations

newtype NodeIndex = NodeIndex Int
  deriving (Eq, Ord)
-- newtype AllocationIndex = AllocationIndex Int
--   deriving (Eq, Ord)
data SomeAllocationIndex where
  SomeAllocationIndex :: AllocationIndex t -> Int -> SomeAllocationIndex
  -- deriving (Eq, Ord)
instance Eq SomeAllocationIndex where
  (==) :: SomeAllocationIndex -> SomeAllocationIndex -> Bool
  (SomeAllocationIndex _ l) == (SomeAllocationIndex _ r) = l == r
instance Ord SomeAllocationIndex where
  (<=) :: SomeAllocationIndex -> SomeAllocationIndex -> Bool
  (SomeAllocationIndex _ l) <= (SomeAllocationIndex _ r) = l <= r
aIdxToInt :: SomeAllocationIndex -> Int
aIdxToInt (SomeAllocationIndex _ v) = v

data AllocationIndex t where
  AllocationIndex :: Maybe t -> AllocationIndex t

newtype EventIndex = EventIndex Int
  deriving (Eq, Ord, Show)

type Nodes = M.Map NodeIndex NodeContent
type Allocations = M.Map SomeAllocationIndex [SomeAllocationIndex]

data EventContents = EventContents [NodeIndex] -- Incoming
                                   [NodeIndex] -- Outgoing
type Events = M.Map EventIndex EventContents

data LHSNode t where
  LHSNodeStart :: NodeIndex -> LHSNode Signal
  LHSNodeEnd :: NodeIndex -> LHSNode SignalResolver
  LHSNodeType :: SomeAllocationIndex -> ScalarType e -> LHSNode t
  LHSNodeSignal :: EventIndex -> LHSNode Signal
  LHSNodeResolver :: EventIndex -> LHSNode SignalResolver
instance Show (LHSNode t) where
  show (LHSNodeStart idx) = "StartNode: " ++ show idx
  show (LHSNodeEnd idx) = "EndNode: " ++ show idx
  show (LHSNodeType idx _) = "TypeNode" ++ show idx
  show (LHSNodeSignal idx) = "SignalNode: " ++ show idx
  show (LHSNodeResolver idx) = "ResolverNode" ++ show idx

data NodeContent = CopyNode [NodeIndex] -- Dependencies
                            SomeAllocationIndex -- From
                            SomeAllocationIndex -- To
                 | Input InOutAllocation
                 | Output InOutAllocation [NodeIndex] -- Dependencies
                 | EmptyNode [NodeIndex] -- Dependencies
  deriving (Show)

data InOutAllocation = AScalar SomeAllocationIndex | ABuffer SomeAllocationIndex
  deriving (Show)

getIdx :: InOutAllocation -> SomeAllocationIndex
getIdx (AScalar idx) = idx
getIdx (ABuffer idx) = idx

addDeps :: NodeContent -> [NodeIndex] -> NodeContent
addDeps (EmptyNode deps) = EmptyNode . (deps ++ )
addDeps (Output a deps) = Output a . (deps ++ )
addDeps (CopyNode deps f t) = \d -> CopyNode (deps ++ d) f t
addDeps (Input a) = const $ Input a


getDeps :: NodeContent -> [NodeIndex]
getDeps (EmptyNode deps) = deps
getDeps (Output _ deps) = deps
getDeps (CopyNode deps _ _) = deps
getDeps (Input _) = []

-- data Exists2 f where
--   Exists2 :: f a b -> Exists2 f

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

convertBody :: Env LHSNode env -> UniformSchedule PTXKernel env -> [NodeIndex] -> Nodes -> Allocations -> Events -> (Nodes, Allocations, Events)
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
  idxIn :: SomeAllocationIndex
  idxIn = case prj' (varIdx rref) env of (LHSNodeType idx _) -> idx
  env' = push' env (lhs, undefined) -- It will only be read here

  idxOut :: SomeAllocationIndex
  idxOut = case prj' (varIdx wref) env' of (LHSNodeType idx _) -> idx

  a' = M.adjust (idxIn :) idxOut a
  nIdx :: NodeIndex
  nIdx = NodeIndex $ M.size m
  m' = M.insert nIdx (CopyNode deps idxIn idxOut) m

  in convertBody env' next [nIdx] m' a' e
convertBody _ _ _ _ _ _ = internalError "Unexpected body contents in schedule. Currently on handles Identity functions"

getDependenciesFromEnv :: Env LHSNode env -> [NodeIndex] -> [Idx env Signal] -> Events -> ([NodeIndex], Events)
getDependenciesFromEnv env deps ss e = (concatMap (\idx -> f $ prj' idx env) ss, updatedEvents)
  where
    f :: LHSNode Signal -> [NodeIndex]
    f (LHSNodeStart i) = [i]
    f (LHSNodeSignal i) = case e M.! i of EventContents ns _ -> ns
    f _ = error "unreachable"

    updatedEvents :: Events
    updatedEvents = foldr (f' . (`prj'` env)) e ss

    f' :: LHSNode Signal -> Events -> Events
    f' (LHSNodeSignal i) m = M.adjust (\(EventContents i' o') -> EventContents (deps ++ i') o') i m
    f' _ m = m

getForwardDependenciesFromEnv :: Env LHSNode env -> [NodeIndex] -> [Idx env SignalResolver] -> Events -> ([NodeIndex], Events)
getForwardDependenciesFromEnv env deps ss e = (concatMap (\idx -> f $ prj' idx env) ss, updatedEvents)
  where


    f :: LHSNode SignalResolver -> [NodeIndex]
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
  let alloc = SomeAllocationIndex (AllocationIndex Nothing) $ M.size a in
  ( ( LHSNodeStart (NodeIndex (M.size m))
    , LHSNodeType (SomeAllocationIndex (AllocationIndex Nothing) (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Input $ AScalar alloc) m
  , M.insert alloc [] a
  )
-- Buffer input argument
convertLHS _ (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref (GroundRbuffer tp))) m a =
  let alloc = SomeAllocationIndex (AllocationIndex Nothing) $ M.size a in
  ( ( LHSNodeStart (NodeIndex (M.size m))
    , LHSNodeType (SomeAllocationIndex (AllocationIndex Nothing) (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Input $ ABuffer alloc) m
  , M.insert alloc [] a
  )
-- Scalar output argument
convertLHS _ (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRscalar tp))) m a =
  let alloc = SomeAllocationIndex (AllocationIndex Nothing) $ M.size a in
  ( ( LHSNodeEnd (NodeIndex (M.size m))
    , LHSNodeType (SomeAllocationIndex (AllocationIndex Nothing) (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Output (AScalar alloc) []) m
  , M.insert alloc [] a
  )
-- Buffer output argument
convertLHS  _ (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRbuffer tp))) m a =
  let alloc = SomeAllocationIndex (AllocationIndex Nothing) $ M.size a in
  ( ( LHSNodeEnd (NodeIndex (M.size m))
    , LHSNodeType (SomeAllocationIndex (AllocationIndex Nothing) (M.size a)) tp
    )
  , M.insert (NodeIndex $ M.size m) (Output (ABuffer alloc) []) m
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

instance Show NodeIndex where
  show (NodeIndex i) = "n" ++ show i
instance Show SomeAllocationIndex where
  show (SomeAllocationIndex _ i) = "d" ++ show i

data HostAllocation where
  ScalarAllocation :: Buffer t -> HostAllocation
  BufferAllocation :: Buffer t -> HostAllocation

withHostPointer :: (Ptr a -> IO b) -> HostAllocation -> IO b
withHostPointer f (ScalarAllocation (Buffer fptr)) = withForeignPtr fptr (f . castPtr)
withHostPointer f (BufferAllocation (Buffer fptr)) = withForeignPtr fptr (f . castPtr)

type InputValues = M.Map SomeAllocationIndex HostAllocation
type OutputAllocations = M.Map SomeAllocationIndex HostAllocation
type OutWrites = M.Map SomeAllocationIndex (MVar () -> OutputAllocations -> IO ())
type DeviceAllocations = M.Map SomeAllocationIndex (CUDA.DevicePtr Word8)


allocateDeviceBuffers :: M.Map SomeAllocationIndex Int -> IO DeviceAllocations
allocateDeviceBuffers = mapM CUDA.mallocArray

allocateHostOutBuffers :: M.Map SomeAllocationIndex Int -> IO OutputAllocations
allocateHostOutBuffers = mapM f
  where
    f :: Int -> IO HostAllocation
    f byteSize = do
      ptr <- mallocForeignPtrBytes byteSize
      return $ BufferAllocation (Buffer ptr)

data Pass1 = Pass1
           { currentIndex :: SomeAllocationIndex
           , inputSizes :: M.Map SomeAllocationIndex Int
           , inputValues :: InputValues
           , outputWriteOps :: OutWrites
           }

incrementIndex :: Pass1 -> Pass1
incrementIndex p@(Pass1 {currentIndex}) = case currentIndex of
  (SomeAllocationIndex x i) -> p{currentIndex = SomeAllocationIndex x (i + 1)}

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
    { currentIndex = SomeAllocationIndex (AllocationIndex Nothing) 0
    , inputSizes = M.empty
    , inputValues = M.empty
    , outputWriteOps = M.empty
    }

  putStrLn $ "Input sizes: \n" ++ prettyPrintMap (inputSizes p1)
  let bytesizes = propagateAllocSizes a (inputSizes p1)
  putStrLn $ "Propagated sizes: \n" ++ prettyPrintMap bytesizes
  -- deviceAllocations <- allocateDeviceBuffers bytesizes

  outputBuffers <- allocateHostOutBuffers bytesizes

  doneMVar <- newEmptyMVar
  doneMVarPtr <- newStablePtrPrimMVar doneMVar

  let
    node_count = M.size n
    n_nodes_c = (fromIntegral node_count :: Word32)
    bytesizes' = [fromIntegral (bytesizes M.! SomeAllocationIndex (AllocationIndex Nothing) i) | i <- [0..(alloc_count-1)]]
    alloc_count = M.size a
    alloc_count_c = (fromIntegral alloc_count :: Word32)
    (node_dependency_counts, node_dependencies) = getNodeDependencies n

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
                    -- TODO instantiate mvars
                    -- mvars <- mapM (const newEmptyMVar) (M.elems a)
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
                      putStrLn "Values before c:"
                      -- mapM_ (\(ptr, size) -> do 
                      --     val <- peek ptr
                      --     print val
                      --     return ()
                      --   ) 
                      --   (zip input_data_ptrs (ascElems (inputSizes p1)))

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


writeNodeContents :: Int -> NodeContent -> Ptr Word32 -> IO ()
writeNodeContents idx nodeContent ptr = case nodeContent of
  (CopyNode _ a1 a2) -> do
    pokeType 0
    pokeA1 a1
    pokeA2 a2
  (Input a) -> do
    pokeType 1
    pokeA1 (getIdx a)
  (Output a _) -> do
    pokeType 2
    pokeA1 (getIdx a)
  (EmptyNode _) -> do
    pokeType 3
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



  -- exec <- Exec.instantiate graph'
  -- Exec.launch exec Stream.defaultStream

  -- DEVICE.sync

  -- mapM_ (\op -> op outputBuffers) $ outputWriteOps p1

constructGraph :: Nodes -> (M.Map NodeIndex Graph.Node -> NodeContent -> IO Graph.Node) -> Graph.Graph -> M.Map NodeIndex Graph.Node -> IO Graph.Graph
constructGraph ns f graph instances =
  if M.null ns then return graph
  else let unblockedNodes = M.filter (all (`M.member` instances) . getDeps) ns in
    if M.null unblockedNodes then return graph
    else do
      newInstances <- mapM (f instances) unblockedNodes

      constructGraph (M.difference ns newInstances) f graph (M.union instances newInstances)

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
  -- ValueBuffer <$> copyToDevice tp (Buffer buffer)
  -- inputHostPtr <- Foreign.mallocBytes byteSize -- :: IO (Ptr t)
  let vs' = M.insert (currentIndex p) (ScalarAllocation (Buffer buffer)) (inputValues p)
  -- Foreign.poke inputHostPtr val

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

writeOutputScalar :: SomeAllocationIndex -> ScalarType e -> IORef e -> M.Map SomeAllocationIndex HostAllocation -> IO ()
writeOutputScalar idx tp ref m = let buf = getValue idx m in writeIORef ref (indexBuffer tp buf 0)

writeOutputBuffer :: SomeAllocationIndex -> IORef (Buffer e) -> M.Map SomeAllocationIndex HostAllocation -> IO ()
writeOutputBuffer idx ref m = let buf = getValue idx m in writeIORef ref buf

getValue :: SomeAllocationIndex -> M.Map SomeAllocationIndex HostAllocation -> Buffer e
getValue idx m = case m M.! idx of BufferAllocation (Buffer p) -> Buffer (castForeignPtr p)
                                   ScalarAllocation (Buffer p) -> Buffer (castForeignPtr p)



propagateAllocSizes :: Allocations -> M.Map SomeAllocationIndex Int -> M.Map SomeAllocationIndex Int
propagateAllocSizes deps sizes | all (`M.member` sizes) (M.keys deps) = sizes
                               | otherwise = let
                                  remainingKeys = filter (\k -> not (M.member k sizes)) (M.keys deps)
                                  withSizes = zip remainingKeys (map (\k -> mapMaybe (sizes M.!?) (deps M.! k)) remainingKeys)
                                  nonEmpty = map (second head) $ filter (not . null . snd) withSizes
                                  in M.union sizes (M.fromList nonEmpty)