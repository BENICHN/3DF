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

cube :: R -> VIBuffer
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
          [ (0, 1, 2),
            (2, 3, 0),
            (4, 5, 6),
            (6, 7, 4),
            (1, 5, 6),
            (6, 2, 1),
            (2, 6, 7),
            (7, 3, 2),
            (0, 3, 7),
            (7, 4, 0),
            (1, 0, 4),
            (4, 5, 1)
          ]
      )

triangle :: Tgl3 -> VIBuffer
triangle (v1, v2, v3) = (G.fromList [v1, v2, v3], G.singleton (0, 1, 2))