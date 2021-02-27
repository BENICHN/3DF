{-# LANGUAGE ViewPatterns #-}

module Geo where

import Foreign
import Vec

type VertexBuffer = BVec Vec3
type IndexBuffer = BVec (Int, Int, Int)
type VIBuffer = (VertexBuffer, IndexBuffer)

transformPC3 :: (Integral i) => i -> i -> Vec3 -> Vec2
transformPC3 w h (lis -> [x, y, z]) =
  let xs = ((1 + (x / z)) * fromIntegral w) / 2
      ys = ((1 - (y / z)) * fromIntegral h) / 2
   in vec [xs, ys]