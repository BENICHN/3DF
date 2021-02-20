module PixArray where

import Colour
import Data.List
import Data.Ratio
import qualified Data.Vector.Generic as G
import Data.Vector.Storable (Vector)
import Utils

data PixelArray = PixelArray Int Int (Vector Colour)

newPixArray :: Integral a => a -> a -> Colour -> PixelArray
newPixArray w' h' c =
  let [w, h] = fromIntegral <$> [w', h']
   in PixelArray w h $ G.concatMap (\y -> G.map (const c) $ G.enumFromTo 1 w) $ G.enumFromTo 1 h

drawLine :: Integral a => Colour -> (a, a) -> (a, a) -> PixelArray -> PixelArray
drawLine c ps@(xs, ys) pe@(xe, ye) arr@(PixelArray w h bits)
  | ps == pe = arr
  | otherwise =
    let dx = xe - xs
        dy = ye - ys

        inv = abs dx < abs dy
        (d1, d2) = swapIf inv (dx, dy)
        (s1, s2) = swapIf inv (xs, ys)

        dd1 = signum d1
        dd2 = d2 % abs d1

        steps = map (\(x1, x2) -> swapIf inv (x1, floor $ fromRatio x2)) . iterate (\(x1, x2) -> (x1 + dd1, x2 + dd2)) $ (s1, s2 % 1)
        entries = filter (\i -> 0 < i && i < G.length bits) . genericTake (abs d1) . map (\(x, y) -> w * fromIntegral y + fromIntegral x) $ steps
     in PixelArray w h $ bits `G.unsafeUpd` zip entries (repeat c)