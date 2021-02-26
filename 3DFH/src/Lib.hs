{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Colour
import Cube
import qualified Data.Vector.Storable as S
import Foreign
import Foreign.C.Types
import PixArray
import Vec

foreign export ccall pixarrayC :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr Colour -> IO ()

pixarrayC :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr Colour -> IO ()
pixarrayC w h mx my t p =
  let -- c = makeRGB t (255 - t) (10 * t)
      arr1 = newPixArray w h black
      a = (fromIntegral t / 60) :: Double
      c = cos a ^ 2
      PixelArray _ _ pixs = draw (cube c) (rz a <> ry a) arr1
   in -- PixelArray _ _ pixs = foldr (\(x, y) -> drawLine c (vec [x, y]) (vec [mx, my])) arr1 [(0, 0), (w `quot` 2, 0), (w, 0), (w, h `quot` 2), (w, h), (w `quot` 2, h), (0, h), (0, h `quot` 2)]
      S.unsafeWith pixs (\pxs -> copyArray p pxs (fromIntegral $ w * h))