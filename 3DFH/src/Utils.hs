module Utils where

import Data.Ratio

swapIf :: Bool -> (a, a) -> (a, a)
swapIf True (x, y) = (y, x)
swapIf False (x, y) = (x, y)

minmax :: Ord a => (a, a) -> (a, a)
minmax (x, y) = swapIf (x > y) (x, y)

fromRatio :: (Real a, RealFrac b) => Ratio a -> b
fromRatio r =
  let n = numerator r
      d = denominator r
   in realToFrac n / realToFrac d

-- floor :: (Real a, Integral b) => a -> b
-- floor x =
--   let r = toRational x
--   in fromInteger $ numerator r `div` denominator r