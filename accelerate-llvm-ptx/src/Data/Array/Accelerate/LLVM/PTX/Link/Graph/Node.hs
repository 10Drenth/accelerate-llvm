{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node where

import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment
import Foreign
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.Kernel
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal
import Debug.Trace (trace)


newtype NIndex = NodeIndex Int
  deriving (Eq, Ord)

instance Show NIndex where
  show (NodeIndex i) = "n" ++ show i

data NContent = CopyNode [NIndex] -- Dependencies
                            MIdx -- From
                            MIdx -- To
                 | Input MIdx
                 | Output MIdx [NIndex] -- Dependencies
                 | EmptyNode [NIndex] -- Dependencies
                --  | AllocNode AIndex -- Target adress
                --              Int -- Element bytesize TODO: Implement (for now ignored)
                --              [AIndex] -- dims TODO: Implement (for now ignored)
                --              [NIndex] -- Dependencies 
                 | KernelNode [NIndex] -- Dependencies
                              KernelNodeContents
  deriving (Show)


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
getDeps (CopyNode deps _ _ ) = deps
getDeps (Input _) = []
-- getDeps (AllocNode _ _ _ deps) = deps
getDeps (KernelNode deps _) = deps

instance MarshalToC NContent where
  marshalSize :: NContent -> Int
  marshalSize _ = marshalSize $ Marshal (MStorable (0 :: Int64), marshalRep emptyKernel)
  marshalAlignment _ = marshalAlignment $ Marshal (MStorable (0 :: Int64), marshalRep emptyKernel)
  marshalWrite ptr v = trace ("Node contents info!!!, size: " ++ show (marshalSize v)) $ case v of
    (CopyNode _ a1 a2)  -> marshalWrite (castPtr ptr) $ Marshal (MStorable (0 :: Int64), Marshal (a1, a2))
    (Input a)           -> marshalWrite (castPtr ptr) $ Marshal (MStorable (1 :: Int64), a)
    (Output a _)        -> marshalWrite (castPtr ptr) $ Marshal (MStorable (2 :: Int64), a)
    (EmptyNode {})      -> marshalWrite (castPtr ptr) $          MStorable (3 :: Int64)
    (KernelNode _ k)    -> marshalWrite (castPtr ptr) $ Marshal (MStorable (5 :: Int64), marshalRep k)
