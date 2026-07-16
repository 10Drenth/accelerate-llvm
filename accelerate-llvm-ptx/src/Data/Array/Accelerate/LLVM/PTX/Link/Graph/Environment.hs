{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment
( memEntries
, memIdxs
, reserveMemory
, reserveBuffer
, maxSizeIndex
, envMemKey
, reserveGround
, prjKernelArgs
, propRef
, GraphEnv (..)
, EventIndex (..)
, GMEntry (..)
, MIdx (..)
, SIdx (..)
, GraphMemory
, MEntryType (..)
) where
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Buffer
import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.Representation.Type
import Data.Data
import LLVM.AST.Type.Representation (makeAligned)
import qualified Data.Map as M
import Data.Array.Accelerate.Representation.Elt
import Foreign
import Data.Array.Accelerate.AST.Environment
import Data.Maybe (maybeToList)
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal (MarshalToC (..))
import Debug.Trace

data GraphEnv t where
  ScalarVal :: MIdx -> !(ScalarType t) -> GraphEnv t
  BufferVal :: MIdx -> GraphEnv (Buffer t)
  RefVal :: GraphEnv t -> GraphEnv (Ref t)
  OutRefVal :: GraphEnv t -> GraphEnv (OutputRef t)
  EventDependency :: EventIndex -> GraphEnv Signal
  EventResolver :: EventIndex -> GraphEnv SignalResolver

envMemKey :: GraphEnv t -> Maybe MIdx
envMemKey (ScalarVal idx _) = Just idx
envMemKey (BufferVal idx) = Just idx
envMemKey (RefVal v) = envMemKey v
envMemKey (OutRefVal v) = envMemKey v
envMemKey _ = Nothing


type GraphMemory = (Int, M.Map MIdx GMEntry)

newtype MIdx = MIdx Int32 -- indices
  deriving (Eq, Ord, Show)
newtype SIdx = SIdx { sIdx :: Int32} -- indices
  deriving (Eq, Ord, Show)
data GMEntry = GMEntry 
  { entryOffset :: Int32 -- offset
  , entrySizeIndex :: SIdx -- Size
  , entryType :: MEntryType -- Scalar or Buffer
  }
  deriving (Eq, Ord, Show)
data MEntryType = MScalar | MBuffer
  deriving (Eq, Ord, Show)

nextSizeIndex :: GraphMemory -> SIdx
nextSizeIndex mem = case maxSizeIndex mem of (SIdx v) -> SIdx $ v + 1 

maxSizeIndex :: GraphMemory -> SIdx
maxSizeIndex (_, m) = SIdx $ 1 + M.foldr f 0 m
  where 
    f e acc = case entrySizeIndex e of (SIdx v) -> max acc v 

memEntries :: GraphMemory -> [GMEntry]
memEntries (_, m) = map snd $ M.toAscList m

memIdxs :: GraphMemory -> [MIdx]
memIdxs (_, m) = map fst $ M.toAscList m

prjKernelArgs :: SArgs env f -> Env GraphEnv env -> [MIdx]
prjKernelArgs ArgsNil _ = []
prjKernelArgs (SArgScalar (Var _ idx) :>: sargs) env = maybeToList (envMemKey (prj' idx env)) ++ prjKernelArgs sargs env
prjKernelArgs (SArgBuffer _ (Var _ idx) :>: sargs) env = maybeToList (envMemKey (prj' idx env)) ++ prjKernelArgs sargs env

reserveMemory :: MEntryType -> Int -> Int -> GraphMemory -> (GraphMemory, MIdx)
reserveMemory tp byteSize al m@(cursor, xs) = let
    cursor' = makeAligned cursor al
    key = MIdx $ fromIntegral $ M.size xs
    sizeIndex = nextSizeIndex m
  in( ( cursor' + byteSize
      , M.insert key (GMEntry (fromIntegral cursor') sizeIndex tp) xs
      )
    , key
    )

reserveBuffer :: GraphMemory -> (GraphMemory, MIdx)
reserveBuffer = reserveMemory MBuffer (sizeOf (0 :: Int)) (sizeOf (0 :: Int))

reserveGround :: GroundR t -> GraphMemory -> (GraphEnv t, GraphMemory, MIdx)
reserveGround (GroundRscalar tp) mem = let
  (sz, al) = scalarTypeSizeAlignment tp
  (m, k) = reserveMemory MScalar sz al mem
  in (ScalarVal k tp, m, k)
reserveGround (GroundRbuffer _) mem = let
  (m, k) = reserveBuffer mem
   in (BufferVal k, m, k)

propRef :: MIdx -> MIdx -> GraphMemory -> Maybe GraphMemory
propRef rIdx wIdx (cursor, xs) = do
  v <- xs M.!? rIdx
  -- TODO: Check values are the same size
  let xs' = M.insert wIdx v xs
  return (cursor, xs')

newtype EventIndex = EventIndex Int
  deriving (Eq, Ord, Show)

instance Distributes GraphEnv where
  reprIsSingle (ScalarVal _ tp) = reprIsSingle tp
  reprIsSingle (BufferVal _) = Refl
  reprIsSingle (EventDependency _) = Refl
  reprIsSingle (EventResolver _) = Refl
  reprIsSingle (RefVal _) = Refl
  reprIsSingle (OutRefVal _) = Refl

  pairImpossible (ScalarVal _ tp) = pairImpossible tp
  unitImpossible (ScalarVal _ tp) = unitImpossible tp

instance MarshalToC MIdx where
  marshalSize = const (sizeOf (0 :: Int32))
  marshalAlignment = const (alignment (0 :: Int32))
  marshalWrite p (MIdx v) = poke (castPtr p) v

instance MarshalToC SIdx where
  marshalSize = const (sizeOf (0 :: Int32))
  marshalAlignment = const (alignment (0 :: Int32))
  marshalWrite p (SIdx v) = poke (castPtr p) v

instance Storable MEntryType where
  sizeOf = const (sizeOf (0 :: Int8))
  alignment = const (alignment (0 :: Int8))
  peek p = do
    (v :: Int8) <- peek (castPtr p)
    return $ case v of
      0 -> MScalar
      1 -> MBuffer
      _ -> error "Invalid entrytype"
  poke p v = poke (castPtr p) $ case v of
    MScalar -> (0 :: Int8)
    MBuffer -> 1