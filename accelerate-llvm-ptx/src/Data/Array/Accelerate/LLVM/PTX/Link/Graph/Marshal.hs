{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal 
( Marshal (..)
, MStorable (..)
, WStorable (..)
, MarshalToC (..)
, MarshalData
, structMarshalData
, toField
) where

import Foreign
import LLVM.AST.Type.Representation (makeAligned)
import Foreign.C.String
import Data.ByteString.Short (ShortByteString, useAsCString)

type Size = Int
type Alignment = Int
data MarshalData = MarshalData
    { sz :: Size
    , al :: Alignment
    , pk :: Ptr () -> IO ()
    }

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

instance MarshalToC (Marshal MarshalData) where
    marshalSize (Marshal x) = sz x
    marshalAlignment (Marshal x) = al x
    marshalWrite ptr (Marshal x) = pk x (castPtr ptr)


structMarshalData :: [MarshalData] -> Marshal MarshalData
structMarshalData xs = let
    structAlignment = foldr (max . al) 0 xs
    dat = marshalStructFields (reverse xs)
    in Marshal MarshalData 
    { sz = makeAligned (sz dat) structAlignment
    , al = structAlignment
    , pk = pk dat
    }

marshalStructFields :: [MarshalData] -> MarshalData
marshalStructFields [] = MarshalData { sz = 0, al = 0, pk = const $ return ()}
marshalStructFields (x:xs) = let
     rec = marshalStructFields xs
     cursor = makeAligned (sz rec) (al x)
     in MarshalData
     { sz = cursor + sz x
     , al = 0 -- Irrelevant
     , pk = \ptr -> do
         pk rec ptr
         pk x (plusPtr ptr cursor)
     }

toField :: MarshalToC a => a -> MarshalData
toField x = MarshalData 
    { sz = marshalSize x
    , al = marshalAlignment x
    , pk = \ptr -> marshalWrite (castPtr ptr) x
    }

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

-- instance (MarshalToC a, MarshalToC b) => MarshalToC (Marshal (a, b)) where
--     marshalSize (Marshal (x, y)) = let
--         al_y = marshalAlignment y
--         in makeAligned (marshalSize x) al_y + marshalSize y
--     marshalAlignment (Marshal (x, y)) = max (marshalAlignment x) (marshalAlignment y)
--     marshalWrite ptr (Marshal (x, y)) = do
--         let xPtr = castPtr $ alignPtr ptr (marshalAlignment x)
--         marshalWrite xPtr x
--         let yPtr = castPtr $ alignPtr (plusPtr xPtr (marshalSize x)) (marshalAlignment y)
--         marshalWrite yPtr y

instance Storable a => MarshalToC (MStorable a) where
    marshalSize (MStorable x) = sizeOf x
    marshalAlignment (MStorable x) = alignment x
    marshalWrite ptr (MStorable x) = poke (castPtr ptr) x

instance MarshalToC a => Storable (WStorable a) where
    sizeOf (WStorable x) = marshalSize x
    alignment (WStorable x) = marshalAlignment x
    peek = error "Marshalled type can not be converted back"
    poke ptr (WStorable x) = marshalWrite (castPtr ptr) x