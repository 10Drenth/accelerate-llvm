{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.Kernel where

import Data.ByteString.Short (ShortByteString)
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment
import Foreign
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal
import Data.Array.Accelerate.LLVM.PTX.Kernel
import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.AST.Environment
import GHC.IO (unsafePerformIO)
import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.PrepKernel
import Data.Array.Accelerate.Lifetime
import Debug.Trace
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Link (KernelObject(kernelObjThreadBlockSize, kernelObjSharedMemBytes))

-- Might need to store modifier here at some point (Read, Write, Mut)
newtype KernelNodeArg = KernelNodeArg MIdx
  deriving (Show)


data KernelNodeContents = KernelNodeContents
  { kernelArgs        :: [KernelNodeArg]
  , mainKernel        :: KernelPhase
  , prepKernel        :: KernelPhase
  }
  deriving (Show)


newtype ModulePath = ModulePath FilePath
  deriving (Show)

newtype KernelSymbol = KernelSymbol ShortByteString
  deriving (Show)

data KernelPhase = KernelPhase
  { modulePath :: FilePath
  , kernelSymbol :: ShortByteString
  , threadBlockSize   :: Int32
  , gridSize          :: Int32
  , sharedMemoryBytes :: Int32
  }
  deriving (Show)

emptyKernel :: KernelNodeContents
emptyKernel = KernelNodeContents
  { kernelArgs = []
  , mainKernel = KernelPhase "" "" 0 0 0
  , prepKernel = KernelPhase "" "" 0 0 0
  }

fromPTXKernel :: Env GraphEnv env -> PTXKernel env' -> SArgs env f -> KernelNodeContents
fromPTXKernel env (PTXKernel {kernelMain, kernelPrepGen}) args = let

      prepObj = unsafePerformIO $ evalPTX defaultTarget $ compilePrep (snd kernelPrepGen) (fst kernelPrepGen) (env, args)

      content = KernelNodeContents
        (KernelNodeArg <$> prjKernelArgs args env)

        (fromPTXKernelPhase kernelMain)
        (KernelPhase (objPath prepObj) (objSym prepObj) 1 1 0)
      in content

fromPTXKernelPhase :: PTXKernelPhase env -> KernelPhase
fromPTXKernelPhase phase = let
  objR = kernelPhaseObject phase
  lObj = unsafeGetValue $ kernelPhaseLinked phase
  in KernelPhase
  { modulePath        = objPath objR
  , kernelSymbol      = objSym objR
  , threadBlockSize   = fromIntegral $ kernelObjThreadBlockSize lObj
  , gridSize          = 256
  , sharedMemoryBytes = fromIntegral $ kernelObjSharedMemBytes lObj
  }


marshalRep
  :: KernelNodeContents
  -> Marshal ( MStorable Word32
   , Marshal ( Marshal [KernelNodeArg]
   , Marshal ( KernelPhase
             , KernelPhase )))
marshalRep (KernelNodeContents args main prep)
  = Marshal ( MStorable $ fromIntegral $ length args
  , Marshal ( Marshal args
  , Marshal ( main
            , prep )))


instance MarshalToC KernelNodeContents where
  marshalSize = marshalSize . marshalRep
  marshalAlignment = marshalAlignment . marshalRep
  marshalWrite ptr = marshalWrite (castPtr ptr) . marshalRep


instance MarshalToC KernelNodeArg where
  marshalSize (KernelNodeArg idx) = marshalSize idx
  marshalAlignment (KernelNodeArg idx) = marshalAlignment idx
  marshalWrite ptr (KernelNodeArg idx) = marshalWrite (castPtr ptr) idx


kernelPhaseMarshalRep :: KernelPhase
  -> Marshal ( FilePath
   , Marshal ( ShortByteString
   , Marshal ( MStorable Word32
   , Marshal ( MStorable Word32
   , MStorable Word32 ))))
kernelPhaseMarshalRep (KernelPhase path symbol tbs gs smem)
  = Marshal ( path
  , Marshal ( symbol
  , Marshal ( MStorable $ fromIntegral tbs
  , Marshal ( MStorable $ fromIntegral gs
            , MStorable $ fromIntegral smem ))))

instance MarshalToC KernelPhase where
  marshalSize = marshalSize . kernelPhaseMarshalRep
  marshalAlignment = marshalAlignment . kernelPhaseMarshalRep
  marshalWrite ptr x = trace ("KernelPhase size and alignment: " ++ show (marshalSize x) ++ ", " ++ show (marshalAlignment x)) $ marshalWrite (castPtr ptr) $ kernelPhaseMarshalRep x
