{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Data.Array.Accelerate.LLVM.PTX.Link.Graph (linkProgram, runGraphProgram, GraphProgram) where

import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.LLVM.PTX.Kernel
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.Type (ScalarType, CChar (CChar))
import Data.Array.Accelerate.Error (internalError)
import qualified Data.Map as M
import Data.Array.Accelerate.AST.Idx
import Control.Concurrent (readMVar, putMVar, takeMVar, forkIO)
import Data.IORef (readIORef, IORef, writeIORef)
import Data.Array.Accelerate.Representation.Elt (showElt, scalarTypeSize)
import Data.Array.Accelerate.Array.Buffer (bufferToList, Buffer (Buffer), memoryByteSize, MutableBuffer (..), newBuffer, writeBuffer, indexBuffer)
import Foreign
import Data.Maybe (mapMaybe, maybeToList)
import Data.Bifunctor
import GHC.Conc (PrimMVar, newStablePtrPrimMVar)
import Control.Concurrent.MVar (newEmptyMVar, MVar)
import Data.Array.Accelerate.AST.Kernel (kernelFunKernel)
import Data.ByteString.Short (ShortByteString, fromShort, useAsCString)
import Data.Array.Accelerate.Lifetime (unsafeGetValue)
import Data.Array.Accelerate.LLVM.PTX.Compile (ObjectR(objPath), objSym, compile)
import Debug.Trace (trace)
import Foreign.C (newCString)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Target
import LLVM.AST.Type.Representation (SizedArray, Struct)
import Data.Array.Accelerate.LLVM.CodeGen.Environment (MarshalEnv)
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base (codeGenKernel)


data GraphProgram = GraphProgram Nodes MemoryMap

newtype NIndex = NodeIndex Int
  deriving (Eq, Ord)
newtype AIndex = AIndex Int
  deriving (Eq, Ord)
aIdxToInt :: AIndex -> Int
aIdxToInt (AIndex v) = v
aIdxToCRep :: AIndex -> Int32
aIdxToCRep (AIndex v) = fromIntegral v

newtype EventIndex = EventIndex Int
  deriving (Eq, Ord, Show)

type Nodes = M.Map NIndex NContent
type MemoryMap = M.Map AIndex [AIndex]

data EventContents = EventContents [NIndex] -- Incoming
                                   [NIndex] -- Outgoing
  deriving (Show)

type Events = M.Map EventIndex EventContents

data GraphEnv t where
  ScalarVal :: AIndex -> !(ScalarType t) -> GraphEnv t
  BufferVal :: AIndex -> GraphEnv (Buffer t)
  RefVal :: GraphEnv t -> GraphEnv (Ref t)
  OutRefVal :: GraphEnv t -> GraphEnv (OutputRef t)
  EventDependency :: EventIndex -> GraphEnv Signal
  EventResolver :: EventIndex -> GraphEnv SignalResolver
-- instance Show (GraphEnv t) where
--   show (InputEvent eIdx nIdx) = "Input event " ++ show eIdx ++ ": " ++ show nIdx
--   show (OutputEvent eIdx nIdx) = "Ouptut event " ++ show eIdx ++ ": " ++ show nIdx
--   show (ScalarVal idx _) = "ScalarVal: " ++ show idx
--   show (BufferVal idx _) = "BufferVal: " ++ show idx
--   show (EventDependency idx) = "EventDependecy: " ++ show idx
--   show (EventResolver idx) = "EventResolver: " ++ show idx
envMemIdx :: GraphEnv t -> Maybe AIndex
envMemIdx (ScalarVal idx _) = Just idx
envMemIdx (BufferVal idx) = Just idx
envMemIdx (RefVal v) = envMemIdx v
envMemIdx (OutRefVal v) = envMemIdx v
envMemIdx _ = Nothing

instance Distributes GraphEnv where
  reprIsSingle (ScalarVal _ tp) = reprIsSingle tp
  reprIsSingle (BufferVal _) = Refl
  reprIsSingle (EventDependency _) = Refl
  reprIsSingle (EventResolver _) = Refl
  reprIsSingle (RefVal _) = Refl
  reprIsSingle (OutRefVal _) = Refl

  pairImpossible (ScalarVal _ tp) = pairImpossible tp
  unitImpossible (ScalarVal _ tp) = unitImpossible tp

data KernelNodeContents = KernelNodeContents
  { readAdresses :: [AIndex]
  , writeAdresses :: [AIndex]
  , modulePath :: FilePath
  , kernelName :: ShortByteString
  }
  deriving (Show)

data NContent = CopyNode [NIndex] -- Dependencies
                            AIndex -- From
                            AIndex -- To
                 | Input AIndex
                 | Output AIndex [NIndex] -- Dependencies
                 | EmptyNode [NIndex] -- Dependencies
                --  | AllocNode AIndex -- Target adress
                --              Int -- Element bytesize TODO: Implement (for now ignored)
                --              [AIndex] -- dims TODO: Implement (for now ignored)
                --              [NIndex] -- Dependencies 
                 | KernelNode [NIndex] -- Dependencies
                              KernelNodeContents
  deriving (Show)

instance Storable NContent where
  alignment = const 8
  sizeOf = const 32
  peek = error "Not implemented"
  poke ptr n = do
    pokeByteOff ptr 0 ntype
    case n of
      (CopyNode _ a1 a2) -> pokeGeneral (aIdxToCRep a1) (aIdxToCRep a2)
      (Input a) -> pokeGeneral (aIdxToCRep a) 0
      (Output a _) -> pokeGeneral (aIdxToCRep a) 0
      (EmptyNode {}) -> pokeGeneral 0 0
      -- (AllocNode {}) -> 4
      (KernelNode _ v) -> pokeByteOff ptr 8 v
    where 
      ntype = cnodeType n

      pokeGeneral :: Int32 -> Int32 -> IO ()
      pokeGeneral a1 a2 = do 
        pokeByteOff ptr 8 a1
        pokeByteOff ptr 16 a2


instance Storable KernelNodeContents where
  alignment = const 8
  sizeOf = const 24
  peek = error "Not implemented"
  poke ptr contents = useAsCString (kernelName contents) $ \symbolC -> do
        pokeByteOff ptr 0 r
        pokeByteOff ptr 4 w
        p <- newCString $ modulePath contents
        pokeByteOff ptr 8 p
        pokeByteOff ptr 16 symbolC
        where
          r = aIdxToCRep $ head $ readAdresses contents 
          w = aIdxToCRep $ head $ writeAdresses contents 

cnodeType :: NContent -> Int8
cnodeType (CopyNode {}) = 0
cnodeType (Input {}) = 1
cnodeType (Output {}) = 2
cnodeType (EmptyNode {}) = 3
-- cnodeType (AllocNode {}) = 4
cnodeType (KernelNode {}) = 5



addDeps :: NContent -> [NIndex] -> NContent
addDeps (EmptyNode deps) = EmptyNode . (deps ++ )
addDeps (Output a deps) = Output a . (deps ++ )
addDeps (CopyNode deps f t) = \d -> CopyNode (deps ++ d) f t
addDeps (Input a) = const $ Input a
-- addDeps (AllocNode a bs dims deps) = AllocNode a bs dims . (deps ++)
addDeps (KernelNode deps c) = \d -> KernelNode (deps ++ d) c


getDeps :: NContent -> [NIndex]
getDeps (EmptyNode deps) = deps
getDeps (Output _ deps) = deps
getDeps (CopyNode deps _ _) = deps
getDeps (Input _) = []
-- getDeps (AllocNode _ _ _ deps) = deps
getDeps (KernelNode deps _) = deps

linkProgram :: UniformScheduleFun PTXKernel () f -> GraphProgram
linkProgram = convertFun

convertFun :: forall t. UniformScheduleFun PTXKernel () t -> GraphProgram
convertFun (Sbody _) = error ""
convertFun (Slam lhs1 (Slam lhs2 f)) = convertFun (Slam (LeftHandSidePair lhs1 lhs2) f)
convertFun (Slam lhs (Sbody body)) = let
  (lhsNodes, s) = convertLHS (lhsToTupR lhs) (ConvState M.empty M.empty M.empty)
  lhsenv = push' Empty (lhs, lhsNodes)
  s' = convertBody s lhsenv body Nothing
  
  nodes'' = processSignalDependencies (nodes s') (events s')
  in trace ("events:" ++ prettyPrintMap (events s')) $ GraphProgram nodes'' (memDeps s')

processSignalDependencies :: Nodes -> Events -> Nodes
processSignalDependencies ns es = foldr f ns (M.elems es)
  where
    f (EventContents incoming outgoing) = M.mapWithKey $
      \nid n -> if nid `elem` outgoing
                then addDeps n incoming
                else n

data ConvState = ConvState 
  { nodes :: Nodes
  , memDeps :: MemoryMap
  , events :: Events
  }

-- convertBody :: Env LHSNode env -> UniformSchedule PTXKernel env -> Maybe NIndex -> Nodes -> MemoryMap -> Events -> (Nodes, MemoryMap, Events)
convertBody :: ConvState -> Env GraphEnv env -> UniformSchedule PTXKernel env -> Maybe NIndex -> ConvState
convertBody s@(ConvState {..}) env schedule prev = let
  nodeIdx = NodeIndex $ M.size nodes
  eventIdx = EventIndex $ M.size events
  memIdx = AIndex $ M.size memDeps
  in case schedule of

  Return -> s

  (Spawn l r) -> let 
    s' = convertBody s env l prev
    in convertBody s' env r prev

  (Effect (SignalAwait signals) Return) -> let
    nodes' = M.insert nodeIdx (EmptyNode (maybeToList prev)) nodes
    events' = updateEvents env nodeIdx signals events
    in s {nodes = nodes', events = events'}

  (Effect (SignalAwait signals) next) -> let
    idx = NodeIndex $ M.size nodes
    events' = updateEvents env idx signals events
    in convertBody s {events = events'} env next prev 

  (Effect (SignalResolve signals) next) -> let
    events' = case prev of
      (Just n) -> updateEvents env n signals events
      Nothing  -> events
    in convertBody s {events = events'} env next prev

  (Alet lhs (NewSignal _) next) -> let
    events' = M.insert eventIdx (EventContents [] []) events
    env' =  push' env (lhs, (EventDependency eventIdx, EventResolver eventIdx))
    in convertBody s {events = events'} env' next prev
  
  (Alet lhs (RefRead rref) (Effect (RefWrite wref _) next)) -> 
    case prj' (varIdx rref) env of 
      (RefVal rval) -> case reprIsSingle @GraphEnv @_ @GraphEnv rval of
        Refl -> let

          idxIn = case envMemIdx rval of 
            (Just v) -> v
            _ -> error "RefRead invalid value"

          env' = push' env (lhs, rval)

          idxOut = case envMemIdx $ prj' (varIdx wref) env' of
            (Just v) -> v
            _ -> error "RefWrite invalid value"

          memDeps' = M.adjust (idxIn :) idxOut memDeps
          nodes' = M.insert nodeIdx (CopyNode (maybeToList prev) idxIn idxOut) nodes

          in convertBody s {nodes = nodes', memDeps = memDeps'} env' next (Just nodeIdx)
      _ -> error "RefRead invalid value"

-- BEGIN TODO
  (Alet lhs (Alloc sh tp vs) next) -> let
    memDeps' = memDeps --M.insert memIdx [] memDeps
    env' = push' env (lhs, BufferVal memIdx)
    in convertBody s {memDeps = memDeps'} env' next prev
  (Alet lhs (RefRead rref) next) ->
    case prj' (varIdx rref) env of 
      (RefVal rval) -> case reprIsSingle @GraphEnv @_ @GraphEnv rval of
        Refl -> let
          env' = push' env (lhs, rval)
          in convertBody s env' next prev
      _ -> error "RefRead invalid value"
  (Effect (RefWrite _ _) next) -> let
    in convertBody s env next prev
  (Effect (Exec metaData fun args) next) -> case kernelFunKernel fun of
    (Exists kernel) -> let
      obj = kernelPhaseObject $ kernelMain kernel
      -- TODO: Get proper in and out adresses
      -- 
      content = KernelNodeContents [AIndex 0] [AIndex 1] (objPath obj) (objSym obj)
      nodes' = M.insert nodeIdx (KernelNode (maybeToList prev) content) nodes
      -- Dependency sets link from first input to first output
      memDeps' = M.adjust (AIndex 0 :) (AIndex 1) memDeps
      in convertBody s {nodes = nodes', memDeps = memDeps'} env next (Just nodeIdx)
-- END TODO

  _ -> error "Unexpected body contents in schedule."

type PrepKernel env = Ptr (SizedArray Word) -> Ptr (Struct (MarshalEnv env)) -> ()

generatePrepareKernel :: PTXKernel a -> LLVM PTX (ObjectR (PrepKernel a))
generatePrepareKernel kernel = do
  _ <- codeGenKernel undefined undefined undefined undefined undefined
  compile undefined undefined undefined undefined


updateEvents :: Env GraphEnv env -> NIndex -> [Idx env t] -> Events -> Events
updateEvents env n ss e = foldr (f . (`prj'` env)) e ss
  where
    f :: GraphEnv t -> Events -> Events
    f (EventDependency eidx) m = M.adjust (\(EventContents incoming outgoing) -> EventContents incoming (n : outgoing)) eidx m
    f (EventResolver eidx) m = M.adjust (\(EventContents incoming outgoing) -> EventContents (n: incoming) outgoing) eidx m
    f _ m = m

convertLHS :: BasesR t -> ConvState -> (Distribute GraphEnv t, ConvState)
convertLHS tup s@(ConvState {..}) = let
  nodeIdx = NodeIndex $ M.size nodes
  eventIdx = EventIndex $ M.size events
  memIdx = AIndex $ M.size memDeps
  in case tup of
-- Unit
  TupRunit -> ((), s)
-- input argument
  (TupRsingle BaseRsignal `TupRpair` TupRsingle (BaseRref scalarOrBuffer)) ->
    ( ( EventDependency eventIdx
      , RefVal $ case scalarOrBuffer of 
        (GroundRscalar tp) -> ScalarVal memIdx tp
        (GroundRbuffer _) ->  BufferVal memIdx
      )
    , s 
      { nodes = M.insert nodeIdx (Input memIdx) nodes
      , memDeps = M.insert memIdx [] memDeps
      , events = M.insert eventIdx (EventContents [nodeIdx] []) events
      } 
    )
-- output argument
  (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite scalarOrBuffer)) ->
    ( ( EventResolver eventIdx
      , OutRefVal $ case scalarOrBuffer of 
        (GroundRscalar tp) -> ScalarVal memIdx tp
        (GroundRbuffer _) -> BufferVal memIdx
      )
    , s 
      { nodes = M.insert nodeIdx (Output memIdx []) nodes
      , memDeps = M.insert memIdx [] memDeps
      , events = M.insert eventIdx (EventContents [] [nodeIdx]) events
      } 
    )
  (TupRpair l r) -> let 
    (vl, s') = convertLHS l s
    (vr, s'') = convertLHS r s' 
    in ((vl, vr), s'')
  _ -> error "Unexpected types in the input or output of an Acc function"

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
  -> Ptr NContent -- Node contents
  -> Word32 -- Alloc count
  -> Ptr Word32 -- input bytesizes
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
  node_contents_fp <- mallocForeignPtrArray $ node_count
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
          pokeArray node_contents_c (ascElems n)

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

  let byteSize = max 1 (scalarTypeSize tp)
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
  let l = bufferToList tp (byteSize' `div` scalarTypeSize tp) val
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



propagateAllocSizes :: MemoryMap -> M.Map AIndex Int -> M.Map AIndex Int
propagateAllocSizes deps sizes | all (`M.member` sizes) (M.keys deps) = sizes
                               | otherwise = let
                                  remainingKeys = filter (\k -> not (M.member k sizes)) (M.keys deps)
                                  withSizes = zip remainingKeys (map (\k -> mapMaybe (sizes M.!?) (deps M.! k)) remainingKeys)
                                  nonEmpty = map (second head) $ filter (not . null . snd) withSizes
                                  in M.union sizes (M.fromList nonEmpty)