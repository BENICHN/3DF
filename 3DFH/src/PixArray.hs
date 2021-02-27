{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module PixArray where

import Colour
import Data.List
import Data.Ratio
import qualified Data.Vector.Generic as G
import Foreign
import Geo
import Utils
import Vec

data PixelArray = PixelArray Int Int (SVec Colour)

newPixArray :: Integral i => i -> i -> Colour -> PixelArray
newPixArray w' h' c =
  let [w, h] = fromIntegral <$> [w', h']
   in PixelArray w h $ G.concatMap (\y -> G.map (const c) $ G.enumFromTo 1 w) $ G.enumFromTo 1 h

putPixels :: Integral i => Colour -> [(i, i)] -> PixelArray -> PixelArray
putPixels c pixs arr@(PixelArray w h bits) =
  let entries = filter (\i -> 0 < i && i < G.length bits) . map (\(x, y) -> w * fromIntegral y + fromIntegral x) $ pixs
   in PixelArray w h $ bits `G.unsafeUpd` zip entries (repeat c)

drawLine :: Colour -> Lin2 -> PixelArray -> PixelArray
drawLine c (ps@((round <$>) . lis -> [xs, ys]), pe@((round <$>) . lis -> [xe, ye]))
  | ps == pe = id
  | otherwise =
    let dx = xe - xs
        dy = ye - ys

        inv = abs dx < abs dy
        (d1, d2) = swapIf inv (dx, dy)
        (s1, s2) = swapIf inv (xs, ys)

        dd1 = signum d1
        dd2 = d2 % abs d1

        steps = genericTake (abs d1) . map (\(x1, x2) -> swapIf inv (x1, floor $ fromRatio x2)) . iterate (\(x1, x2) -> (x1 + dd1, x2 + dd2)) $ (s1, s2 % 1)
     in putPixels c steps

drawFlatTriangle :: Bool -> Colour -> Tgl2 -> PixelArray -> PixelArray
drawFlatTriangle ft c (lis -> [x1l, y1l], lis -> [x1r, y1r], lis -> [x2, y2]) =
  -- nodeconstruct
  let y1 = y1l
      dy = abs $ y2 - y1

      dxl = x2 - x1l
      dxr = x2 - x1r

      ddxl = dxl / dy
      ddxr = dxr / dy

      ys = if ft then enumFromTo (floor y1) (ceiling y2) else enumFromDownto (ceiling y1) (floor y2)
      xls = iterate (+ ddxl) x1l
      xrs = iterate (+ ddxr) x1r

      lines = zipWith (\xl xr -> [round xl .. round xr]) xls xrs
      pixs = concat . drop (if ft `xor` (frac y1 <= 0.5) then 1 else 0) $ zipWith (\xs y -> zip xs (repeat y)) lines ys
   in putPixels c pixs

drawTriangle :: Colour -> Tgl2 -> PixelArray -> PixelArray
drawTriangle c (p1, p2, p3) =
  let [pt@(lis -> [xt, yt]), pm@(lis -> [xm, ym]), pb@(lis -> [xb, yb])] = sortBy (\p q -> compare (G.reverse p) (G.reverse q)) [p1, p2, p3]
   in if
          | yt == ym && ym == yb -> id
          | yt == ym -> drawFlatTriangle True c (pt, pm, pb)
          | ym == yb -> drawFlatTriangle False c (pm, pb, pt)
          | otherwise ->
            let a = (xt - xb) / (yt - yb)
                xi = a * (ym - yt) + xt
                pi = vec [xi, ym]
                (pl, pr) = swapIf (xi < xm) (pm, pi)
             in drawFlatTriangle True c (pl, pr, pb) . drawFlatTriangle False c (pl, pr, pt)

draw :: VIBuffer -> Mat3 -> PixelArray -> PixelArray
draw (vb, ib) tr pixArr@(PixelArray w h _) =
  let vb' = G.map (transformPC3 w h . (+ vec [0, 0, 3]) . (tr #>)) vb
      drawITgl (i1, i2, i3) =
        let p1 = vb' G.! i1
            p2 = vb' G.! i2
            p3 = vb' G.! i3
         in drawLine white (p1, p2) .
            drawLine white (p2, p3) .
            drawLine white (p3, p1) .
            drawTriangle red (p1, p2, p3)
   in G.foldr drawITgl pixArr ib