{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.Kernel where

import Data.ByteString.Short (ShortByteString)
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment
import Foreign
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal

-- Might need to store modifier here at some point (Read, Write, Mut)
newtype KernelNodeArg = KernelNodeArg MIdx
  deriving (Show)


data KernelNodeContents = KernelNodeContents
  { kernelArgs :: [KernelNodeArg]
  , mainKernel :: KernelPhase
  , prepKernel :: KernelPhase
  }
  deriving (Show)


newtype ModulePath = ModulePath FilePath
  deriving (Show)

newtype KernelSymbol = KernelSymbol ShortByteString
  deriving (Show)


data KernelPhase = KernelPhase
  { modulePath :: FilePath
  , kernelSymbol :: ShortByteString
  }
  deriving (Show)

emptyKernel :: KernelNodeContents
emptyKernel = KernelNodeContents [] (KernelPhase "" "") (KernelPhase "" "")

marshalRep 
  :: KernelNodeContents 
  -> Marshal 
  ( MStorable Word32
  , Marshal 
    ( Marshal [KernelNodeArg]
      , Marshal 
        ( KernelPhase
        , KernelPhase
        )
    )
  )
marshalRep (KernelNodeContents args main prep) =
  Marshal 
  ( MStorable (fromIntegral $ length args)
  , Marshal 
    ( Marshal args
    , Marshal 
      ( main
      , prep
      )
    )
  )
 
instance MarshalToC KernelNodeContents where
  marshalSize = marshalSize . marshalRep
  marshalAlignment = marshalAlignment . marshalRep
  marshalWrite ptr = marshalWrite (castPtr ptr) . marshalRep 


instance MarshalToC KernelNodeArg where
  marshalSize (KernelNodeArg idx) = marshalSize idx
  marshalAlignment (KernelNodeArg idx) = marshalAlignment idx
  marshalWrite ptr (KernelNodeArg idx) = marshalWrite (castPtr ptr) idx


instance MarshalToC KernelPhase where
  marshalSize (KernelPhase path symbol) = marshalSize $ Marshal (path, symbol)
  marshalAlignment (KernelPhase path symbol) = marshalAlignment $ Marshal (path, symbol)
  marshalWrite ptr (KernelPhase path symbol) = marshalWrite (castPtr ptr) $ Marshal (path, symbol)
