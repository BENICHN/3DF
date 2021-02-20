{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import Foreign
import Foreign.C.Types

type Colour = CLong

getARGB :: Integral a => Colour -> (a, a, a, a)
getARGB c =
  let [a, r, g, b] = fromIntegral . (.&. 0xFF) <$> take 4 (iterate (`shiftR` 2) c)
   in (a, r, g, b)

makeARGB :: Integral a => a -> a -> a -> a -> Colour
makeARGB a r g b = fromIntegral a * 0x1000000 .|. fromIntegral r * 0x10000 .|. fromIntegral g * 0x100 .|. fromIntegral b

makeRGB :: Integral a => a -> a -> a -> Colour
makeRGB = makeARGB 0xFF

pixarray :: (Storable a, Integral a) => a -> a -> a -> Vector Colour
pixarray w h t = S.concatMap (\y -> S.map (\x -> makeRGB x y t) $ S.enumFromTo 1 w) $ S.enumFromTo 1 h

foreign export ccall pixarrayC :: CInt -> CInt -> CInt -> Ptr CLong -> IO ()

pixarrayC :: CInt -> CInt -> CInt -> Ptr CLong -> IO ()
pixarrayC w h t p =
  let pixs = pixarray w h t
   in S.unsafeWith pixs $ \pxs -> copyArray p pxs (fromIntegral $ w * h)