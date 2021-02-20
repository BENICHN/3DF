module Colour where

import Data.Bits
import Foreign.C.Types

type Colour = CLong

black, red, green, blue, white :: Colour
black = 0x00000000
red = 0x0000FF00
green = 0x00FF0000
blue = 0xFF000000
white = 0xFFFFFF00

getARGB :: Integral a => Colour -> (a, a, a, a)
getARGB c =
  let [b, g, r, a] = fromIntegral . (.&. 0xFF) <$> take 4 (iterate (`shiftR` 2) c)
   in (a, r, g, b)

makeARGB :: Integral a => a -> a -> a -> a -> Colour
makeARGB a r g b = fromIntegral b * 0x1000000 .|. fromIntegral g * 0x10000 .|. fromIntegral r * 0x100 .|. fromIntegral a

makeRGB :: Integral a => a -> a -> a -> Colour
makeRGB = makeARGB 0xFF