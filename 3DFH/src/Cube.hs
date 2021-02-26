{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Cube where

import Colour
import qualified Data.Vector.Generic as G
import Foreign
import Geo
import PixArray
import Utils
import Vec

cube :: (RealFrac a, Storable a) => a -> VIBuffer a
cube cot =
  let c = cot / 2
   in ( G.fromList
          [ vec [- c, - c, - c],
            vec [c, - c, - c],
            vec [c, c, - c],
            vec [- c, c, - c],
            vec [- c, - c, c],
            vec [c, - c, c],
            vec [c, c, c],
            vec [- c, c, c]
          ],
        G.fromList
          [ vec [0, 1],
            vec [1, 2],
            vec [2, 3],
            vec [3, 0],
            vec [4, 5],
            vec [5, 6],
            vec [6, 7],
            vec [7, 4],
            vec [0, 4],
            vec [1, 5],
            vec [2, 6],
            vec [3, 7]
          ]
      )