{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Colour
import Cube
import qualified Data.Vector.Storable as S
import Foreign
import Foreign.C.Types
import PixArray
import SceneState
import Vec

foreign export ccall pixarrayC :: Ptr SceneState -> Ptr Colour -> IO ()

pixarrayC :: Ptr SceneState -> Ptr Colour -> IO ()
pixarrayC pss p = do
  ss <- peek pss
  let w = width ss
      h = height ss
      t = fromIntegral (frameCount ss) / 60 :: R
      c = 1
      fig = cube c -- triangle (vec [0, 0, 0], vec [1, 0, 0], vec [0.5, 1, 0])
      m = rz t <> ry t
      PixelArray _ _ pixs = draw fig m $ newPixArray w h black
  S.unsafeWith pixs (\pxs -> copyArray p pxs (fromIntegral $ w * h))