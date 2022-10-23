{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World
  ( World(..)
  , initialWorld
  ) where

import MainScene.MainScene
import ImageRsc (ImageRsc)
import qualified SDL
import Vec (VecInt)
import FontRsc (FontRsc)


data World = World
  { exiting :: Bool
  , baseFps :: Int
  , scene :: MainScene
  , window :: SDL.Window
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  , fontRsc :: FontRsc
  , windowSize :: VecInt
  }


initialWorld :: SDL.Window -> SDL.Renderer -> ImageRsc -> FontRsc -> VecInt -> World
initialWorld window' renderer' imageRsc' fontRsc' windowSize' = World
  { exiting = False
  , baseFps = 60
  , scene = initialMainScene windowSize'
  , window = window'
  , renderer = renderer'
  , imageRsc = imageRsc'
  , fontRsc = fontRsc'
  , windowSize = windowSize'
  }
