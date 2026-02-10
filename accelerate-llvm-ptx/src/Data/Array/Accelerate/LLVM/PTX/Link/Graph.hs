{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Array.Accelerate.LLVM.PTX.Link.Graph (linkProgram, inspectAllocSizes, GraphProgram) where

import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.LLVM.PTX.Kernel (PTXKernel)
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.Type (ScalarType, Word64)
import Data.Array.Accelerate.Error (internalError)
import qualified Data.Map as M
import Data.Array.Accelerate.AST.Idx
import Control.Concurrent (readMVar, putMVar)
import Data.IORef (readIORef, IORef, writeIORef)
import Data.Array.Accelerate.Representation.Elt (showElt, bytesElt)
import Data.Array.Accelerate.Array.Buffer (bufferToList, Buffer (Buffer), memoryByteSize, MutableBuffer (..), newBuffer, writeBuffer, indexBuffer)
import Foreign
import Data.Maybe (mapMaybe)
import Data.Bifunctor
import qualified Foreign.CUDA as DEVICE
import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Graph.Build as Graph
import qualified Foreign.CUDA.Driver.Graph.Exec as Exec
import qualified Foreign.CUDA.Driver.Stream as Stream


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
type OutWrites = M.Map SomeAllocationIndex (OutputAllocations -> IO ())
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

inspectAllocSizes :: GraphProgram -> TupR BaseR t -> t -> IO ()
inspectAllocSizes (GraphProgram n a) tup v = do

  -- let ctx = unsafeGetValue $ deviceContext $ ptxContext defaultTarget

  CUDA.initialise []
  dev0 <- CUDA.device 0
  ctx <- CUDA.create dev0 []
  p1 <- inspectInputAllocSizes tup v $ Pass1
    { currentIndex = SomeAllocationIndex (AllocationIndex Nothing) 0
    , inputSizes = M.empty
    , inputValues = M.empty
    , outputWriteOps = M.empty
    }

  putStrLn $ "Input sizes: \n" ++ prettyPrintMap (inputSizes p1)
  let bytesizes = propagateAllocSizes a (inputSizes p1)
  putStrLn $ "Propagated sizes: \n" ++ prettyPrintMap bytesizes
  deviceAllocations <- allocateDeviceBuffers bytesizes

  outputBuffers <- allocateHostOutBuffers bytesizes

  graph <- Graph.create []

  let f :: M.Map NodeIndex Graph.Node -> NodeContent -> IO Graph.Node
      f acc (EmptyNode deps) = Graph.addEmpty graph (mapMaybe (acc M.!?) deps)
      f _   (Input alloc) = let
        byteSize = bytesizes M.! getIdx alloc
        input = inputValues p1 M.! getIdx alloc
        inputDevicePtr = CUDA.useDevicePtr $ CUDA.castDevPtr $ deviceAllocations M.! getIdx alloc
        in withHostPointer (\inputHostPtr -> Graph.addMemcpy graph [] ctx
          0 0 0 0 CUDA.HostMemory inputHostPtr byteSize 1
          0 0 0 0 CUDA.DeviceMemory inputDevicePtr byteSize 1
          byteSize 1 1) input
      f acc (Output alloc deps) = let
        depInstances = mapMaybe (acc M.!?) deps
        byteSize = bytesizes M.! getIdx alloc
        output = outputBuffers M.! getIdx alloc
        outputDevicePtr = CUDA.useDevicePtr $ CUDA.castDevPtr $ deviceAllocations M.! getIdx alloc
        in withHostPointer (\outputHostPtr -> Graph.addMemcpy graph depInstances ctx
          0 0 0 0 CUDA.DeviceMemory outputDevicePtr byteSize 1
          0 0 0 0 CUDA.HostMemory outputHostPtr byteSize 1
          byteSize 1 1) output
      f acc (CopyNode deps idxIn idxOut) = let
        depInstances = mapMaybe (acc M.!?) deps
        byteSizeIn = bytesizes M.! idxIn
        byteSizeOut = bytesizes M.! idxOut
        devPtrIn = CUDA.useDevicePtr $ CUDA.castDevPtr $ deviceAllocations M.! idxIn
        devPtrOut = CUDA.useDevicePtr $ CUDA.castDevPtr $ deviceAllocations M.! idxOut
        in Graph.addMemcpy graph depInstances ctx
          0 0 0 0 CUDA.DeviceMemory devPtrIn byteSizeIn 1
          0 0 0 0 CUDA.DeviceMemory devPtrOut byteSizeOut 1
          byteSizeOut 1 1

  graph' <- constructGraph n f graph M.empty



  exec <- Exec.instantiate graph'
  Exec.launch exec Stream.defaultStream

  DEVICE.sync

  mapM_ (\op -> op outputBuffers) $ outputWriteOps p1

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
    f :: OutputAllocations -> IO ()
    f outBufs = do
     writeOutputScalar (currentIndex p) tp output outBufs
     putMVar mvar ()

  let writes' = M.insert (currentIndex p) f (outputWriteOps p)
  return (incrementIndex (Pass1 {currentIndex = currentIndex p, inputSizes = inputSizes p, inputValues = inputValues p, outputWriteOps = writes'}))
-- Buffer output argument
inspectInputAllocSizes (TupRsingle BaseRsignalResolver `TupRpair` TupRsingle (BaseRrefWrite (GroundRbuffer _))) (SignalResolver mvar, OutputRef output) p = do
  let
    f :: OutputAllocations -> IO ()
    f outBufs = do
     writeOutputBuffer (currentIndex p) output outBufs
     putMVar mvar ()

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