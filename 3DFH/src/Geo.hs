{-# LANGUAGE ViewPatterns #-}

module Geo where

import Vec
import Foreign

type VertexBuffer a = BVec (Vec3 a)
type IndexBuffer = BVec (Vec2 Int)
type VIBuffer a = (VertexBuffer a, IndexBuffer)

transformPC3 :: (Storable a, RealFrac a, Integral i) => i -> i -> Vec3 a -> Vec2 Int
transformPC3 w h (lis -> [x, y, z]) =
  let xs = floor ((1 + x) * fromIntegral w) `div` 2
      ys = floor ((1 - y) * fromIntegral h) `div` 2
   in vec [xs, ys]