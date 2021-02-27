{-# LINE 1 "SceneState.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SceneState where

import Foreign
import Foreign.C.Types
import Graphics.Win32



data SceneState = SceneState 
  { width :: CInt,
    height :: CInt,
    fps :: CInt,
    frameCount :: CInt,
    lButton :: Bool,
    mButton :: Bool,
    rButton :: Bool,
    mouseX :: CInt,
    mouseY :: CInt,
    keysPressed :: VKey
  }

instance Storable SceneState where
    sizeOf _ = ((32))
{-# LINE 26 "SceneState.hsc" #-}
    alignment _ = 8
    peek p = do
        width <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 29 "SceneState.hsc" #-}
        height <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 30 "SceneState.hsc" #-}
        fps <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 31 "SceneState.hsc" #-}
        frameCount <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 32 "SceneState.hsc" #-}
        lButton <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 33 "SceneState.hsc" #-}
        mButton <- ((\hsc_ptr -> peekByteOff hsc_ptr 17)) p
{-# LINE 34 "SceneState.hsc" #-}
        rButton <- ((\hsc_ptr -> peekByteOff hsc_ptr 18)) p
{-# LINE 35 "SceneState.hsc" #-}
        mouseX <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p
{-# LINE 36 "SceneState.hsc" #-}
        mouseY <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 37 "SceneState.hsc" #-}
        keysPressed <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) p
{-# LINE 38 "SceneState.hsc" #-}
        return SceneState {
            width = width,
            height = height,
            fps = fps,
            frameCount = frameCount,
            lButton = lButton,
            mButton = mButton,
            rButton = rButton,
            mouseX = mouseX,
            mouseY = mouseY,
            keysPressed = keysPressed
        }
    poke p v = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (width v)
{-# LINE 52 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p (height v)
{-# LINE 53 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (fps v)
{-# LINE 54 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p (frameCount v)
{-# LINE 55 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (lButton v)
{-# LINE 56 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 17)) p (mButton v)
{-# LINE 57 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 18)) p (rButton v)
{-# LINE 58 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) p (mouseX v)
{-# LINE 59 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p (mouseY v)
{-# LINE 60 "SceneState.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) p (keysPressed v)