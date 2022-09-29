{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Main (main) where

import qualified SDL
import qualified SDLUtil


main :: IO ()
main = SDLUtil.withSDL $ SDLUtil.withWindow "Hello Seed" (640, 480) $
  \w -> do
    screen <- SDL.getWindowSurface w
    -- pixelFormat <- SDL.surfaceFormat `applyToPointer` screen
    -- color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF
    SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
    SDL.updateWindowSurface w
    SDL.delay 2000
    SDL.freeSurface screen