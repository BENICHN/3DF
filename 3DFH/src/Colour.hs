module Colour where

import Data.Bits
import Foreign.C.Types

type Colour = CLong

black, red, green, blue, white :: Colour
black = 0x00000000
red = 0x00FF0000
green = 0x0000FF00
blue = 0x000000FF
white = 0x00FFFFFF

getARGB :: Integral a => Colour -> (a, a, a, a)
getARGB c =
  let [a, r, g, b] = fromIntegral . (.&. 0xFF) <$> take 4 (iterate (`shiftR` 2) c)
   in (a, r, g, b)

makeARGB :: Integral a => a -> a -> a -> a -> Colour
makeARGB a r g b = fromIntegral a * 0x1000000 .|. fromIntegral r * 0x10000 .|. fromIntegral g * 0x100 .|. fromIntegral b

makeRGB :: Integral a => a -> a -> a -> Colour
makeRGB = makeARGB 0xFF