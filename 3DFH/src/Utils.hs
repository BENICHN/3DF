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

frac :: RealFrac a => a -> a
frac x = let (n, t) = properFraction x in t

enumFromDownto :: (Enum a, Ord a) => a -> a -> [a]
enumFromDownto s e
  | s >= e = s : enumFromDownto (pred s) e
  | otherwise = []