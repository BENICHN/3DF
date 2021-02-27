{-# LANGUAGE ForeignFunctionInterface #-}

module SceneState where

import Foreign
import Foreign.C.Types
import Graphics.Win32

#include "SceneState.h"

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
    sizeOf _ = (#size SceneState)
    alignment _ = 8
    peek p = do
        width <- (#peek SceneState, width) p
        height <- (#peek SceneState, height) p
        fps <- (#peek SceneState, fps) p
        frameCount <- (#peek SceneState, frameCount) p
        lButton <- (#peek SceneState, lButton) p
        mButton <- (#peek SceneState, mButton) p
        rButton <- (#peek SceneState, rButton) p
        mouseX <- (#peek SceneState, mouseX) p
        mouseY <- (#peek SceneState, mouseY) p
        keysPressed <- (#peek SceneState, keysPressed) p
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
        (#poke SceneState, width) p (width v)
        (#poke SceneState, height) p (height v)
        (#poke SceneState, fps) p (fps v)
        (#poke SceneState, frameCount) p (frameCount v)
        (#poke SceneState, lButton) p (lButton v)
        (#poke SceneState, mButton) p (mButton v)
        (#poke SceneState, rButton) p (rButton v)
        (#poke SceneState, mouseX) p (mouseX v)
        (#poke SceneState, mouseY) p (mouseY v)
        (#poke SceneState, keysPressed) p (keysPressed v)