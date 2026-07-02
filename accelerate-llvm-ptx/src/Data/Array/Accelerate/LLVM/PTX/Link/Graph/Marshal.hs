{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal where

import Foreign
import LLVM.AST.Type.Representation (makeAligned)
import Foreign.C.String
import Data.ByteString.Short (ShortByteString, useAsCString)

type MarshalData a = (Int, Int, Ptr a -> IO (), Maybe a)

newtype Marshal a = Marshal a
newtype (Storable a) => MStorable a = MStorable a
newtype (MarshalToC a) => WStorable a = WStorable a 
    

class MarshalToC a where
    marshalSize :: a -> Int
    marshalAlignment :: a -> Int
    marshalWrite :: Ptr a -> a -> IO ()

instance MarshalToC ShortByteString where
  marshalSize = const $ sizeOf (0 :: Int)
  marshalAlignment = const $ alignment (0 :: Int)
  marshalWrite ptr str = useAsCString str $ \strC -> do
    poke (castPtr ptr) strC

instance MarshalToC FilePath where
  marshalSize = const $ sizeOf (0 :: Int)
  marshalAlignment = const $ alignment (0 :: Int)
  marshalWrite ptr pth = do
    pthC <- newCString pth
    poke (castPtr ptr) pthC


instance MarshalToC a => MarshalToC (Marshal [a]) where
    marshalSize = const $ sizeOf (0 :: Int)
    marshalAlignment = const $ alignment (0 :: Int)
    marshalWrite ptr (Marshal xs) = let
        xs' = WStorable <$> xs
        in do
        xsFp <- mallocForeignPtrArray (length xs)
        withForeignPtr xsFp $ \xsPtr -> do
            pokeArray xsPtr xs'
            poke (castPtr ptr) xsPtr

instance (MarshalToC a, MarshalToC b) => MarshalToC (Marshal (a, b)) where
    marshalSize v@(Marshal (x, y)) = makeAligned (makeAligned (marshalSize x) (marshalAlignment y) + marshalSize y) (marshalAlignment v)
    marshalAlignment (Marshal (x, y)) = max (marshalAlignment x) (marshalAlignment y)
    marshalWrite ptr (Marshal (x, y)) = do
        let xPtr = castPtr $ alignPtr ptr (marshalAlignment x) 
        marshalWrite xPtr x
        let yPtr = castPtr $ alignPtr (plusPtr xPtr (marshalSize x)) (marshalAlignment y)
        marshalWrite yPtr y

instance Storable a => MarshalToC (MStorable a) where
    marshalSize (MStorable x) = sizeOf x
    marshalAlignment (MStorable x) = alignment x
    marshalWrite ptr (MStorable x) = poke (castPtr ptr) x

instance MarshalToC a => Storable (WStorable a) where
    sizeOf (WStorable x) = marshalSize x
    alignment (WStorable x) = marshalAlignment x
    peek = error "Marshalled type can not be converted back"
    poke ptr (WStorable x) = marshalWrite (castPtr ptr) x