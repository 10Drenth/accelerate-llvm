{-# LANGUAGE GADTs #-}


module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment where
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Buffer
import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.Representation.Type
import Data.Data

data GraphEnv t where
  ScalarVal :: AIndex -> !(ScalarType t) -> GraphEnv t
  BufferVal :: AIndex -> GraphEnv (Buffer t)
  RefVal :: GraphEnv t -> GraphEnv (Ref t)
  OutRefVal :: GraphEnv t -> GraphEnv (OutputRef t)
  EventDependency :: EventIndex -> GraphEnv Signal
  EventResolver :: EventIndex -> GraphEnv SignalResolver

newtype EventIndex = EventIndex Int
  deriving (Eq, Ord, Show)

newtype AIndex = AIndex Int
  deriving (Eq, Ord)

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

instance Show AIndex where
  -- show (SomeAllocationIndex _ i) = "d" ++ show i
  show (AIndex i) = "d" ++ show i

instance Distributes GraphEnv where
  reprIsSingle (ScalarVal _ tp) = reprIsSingle tp
  reprIsSingle (BufferVal _) = Refl
  reprIsSingle (EventDependency _) = Refl
  reprIsSingle (EventResolver _) = Refl
  reprIsSingle (RefVal _) = Refl
  reprIsSingle (OutRefVal _) = Refl

  pairImpossible (ScalarVal _ tp) = pairImpossible tp
  unitImpossible (ScalarVal _ tp) = unitImpossible tp