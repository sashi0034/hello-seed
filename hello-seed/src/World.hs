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


data World = World
  { exiting :: Bool
  , baseFps :: Int
  , scene :: MainScene
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  , windowSize :: VecInt
  }


initialWorld :: SDL.Renderer -> ImageRsc -> VecInt -> World
initialWorld renderer' imageRsc' windowSize' = World
  { exiting = False
  , baseFps = 60
  , scene = initialMainScene windowSize'
  , renderer = renderer'
  , imageRsc = imageRsc'
  , windowSize = windowSize'
  }
