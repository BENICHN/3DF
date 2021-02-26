module Vec where

import Foreign
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Numeric.LinearAlgebra as L

type UVec = U.Vector
type SVec = S.Vector
type BVec = V.Vector

type Vec2 = L.Vector
type Vec3 = L.Vector
type Vec4 = L.Vector

vec :: Storable a => [a] -> L.Vector a
vec = L.fromList

lis :: Storable a => L.Vector a -> [a]
lis = L.toList