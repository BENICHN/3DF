module Vec (module Vec, module L) where

import Foreign
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Numeric.LinearAlgebra as L hiding ((<>))

type UVec = U.Vector
type SVec = S.Vector
type BVec = V.Vector

type Vec2 = L.Vector
type Vec3 = L.Vector
type Vec4 = L.Vector

type Mat2 = L.Matrix
type Mat3 = L.Matrix
type Mat4 = L.Matrix

vec :: Storable a => [a] -> L.Vector a
vec = L.fromList

lis :: Storable a => L.Vector a -> [a]
lis = L.toList

rx :: (Storable a, Floating a) => a -> Matrix a
rx a = let 
  s = sin a
  c = cos a
  in (3><3) [ 1, 0 , 0,
              0, c, -s,
              0, s,  c ]

ry :: (Storable a, Floating a) => a -> Matrix a
ry a = let 
  s = sin a
  c = cos a
  in (3><3) [  c, 0, s,
               0, 1, 0,
              -s, 0, c ]

rz :: (Storable a, Floating a) => a -> Matrix a
rz a = let 
  s = sin a
  c = cos a
  in (3><3) [ c, -s, 0,
              s,  c, 0, 
              0,  0, 1 ]