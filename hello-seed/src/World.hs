{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World
  ( World(..)
  , initialWorld
  , baseFps
  ) where

import MainScene.MainScene
import ImageRsc (ImageRsc)
import qualified SDL
import Vec (VecInt)
import FontRsc (FontRsc)
import InputState (InputState, noInput)


data World = World
  { exiting :: Bool
  , currentBaseFps :: Int
  , scene :: MainScene
  , window :: SDL.Window
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  , fontRsc :: FontRsc
  , windowSize :: VecInt
  , input :: InputState
  }


baseFps :: Int
baseFps = 60


initialWorld :: SDL.Window -> SDL.Renderer -> ImageRsc -> FontRsc -> VecInt -> MainScene -> World
initialWorld window' renderer' imageRsc' fontRsc' windowSize' initialScene = World
  { exiting = False
  , currentBaseFps = baseFps
  , scene = initialScene
  , window = window'
  , renderer = renderer'
  , imageRsc = imageRsc'
  , fontRsc = fontRsc'
  , windowSize = windowSize'
  , input = noInput
  }
