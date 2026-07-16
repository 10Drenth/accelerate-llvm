{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node where

import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment
import Foreign
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.Kernel
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal
import Debug.Trace (trace)
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.AllocKernel (AllocNode, allocNodeMarshalRep)

type NodeDependencies = [NIndex]

newtype NIndex = NodeIndex Int
  deriving (Eq, Ord)

instance Show NIndex where
  show (NodeIndex i) = "n" ++ show i

data NContent = CopyNode NodeDependencies -- Dependencies
                            MIdx -- From
                            MIdx -- To
                 | Input MIdx
                 | Output MIdx NodeDependencies
                 | EmptyNode NodeDependencies
                 | AllocNode NodeDependencies AllocNode
                 | KernelNode NodeDependencies KernelNodeContents
  deriving (Show)


addDeps :: NContent -> [NIndex] -> NContent
addDeps (EmptyNode deps) = EmptyNode . (deps ++ )
addDeps (Output a deps) = Output a . (deps ++ )
addDeps (CopyNode deps f t) = \d -> CopyNode (deps ++ d) f t
addDeps (Input a) = const $ Input a
addDeps (AllocNode deps c) = \d -> AllocNode (deps ++ d) c
addDeps (KernelNode deps c) = \d -> KernelNode (deps ++ d) c


getDeps :: NContent -> [NIndex]
getDeps (EmptyNode deps) = deps
getDeps (Output _ deps) = deps
getDeps (CopyNode deps _ _ ) = deps
getDeps (Input _) = []
getDeps (AllocNode deps _) = deps
getDeps (KernelNode deps _) = deps

instance MarshalToC NContent where
  marshalSize :: NContent -> Int
  marshalSize _ = marshalSize $ structMarshalData 
    [ toField $ MStorable (0 :: Int32)
    , toField $ kernelNodeMarshalRep emptyKernel
    ]
  marshalAlignment _ = marshalAlignment $ structMarshalData 
    [ toField $ MStorable (0 :: Int32)
    , toField $ kernelNodeMarshalRep emptyKernel
    ]
  marshalWrite ptr v = let
    rep = trace ("Node contents info!!!, size: " ++ show (marshalSize v)) $ case v of
      (CopyNode _ a1 a2)  -> structMarshalData [ toField $ MStorable (0 :: Int32), toField a1, toField a2 ]
      (Input a)           -> structMarshalData [ toField $ MStorable (1 :: Int32), toField a ]
      (Output a _)        -> structMarshalData [ toField $ MStorable (2 :: Int32), toField a ]
      (EmptyNode {})      -> structMarshalData [ toField $ MStorable (3 :: Int32) ]
      (AllocNode _ a)     -> structMarshalData [ toField $ MStorable (4 :: Int32), toField (allocNodeMarshalRep a) ]
      (KernelNode _ k)    -> structMarshalData [ toField $ MStorable (5 :: Int32), toField (kernelNodeMarshalRep k) ]
    in marshalWrite (castPtr ptr) rep
