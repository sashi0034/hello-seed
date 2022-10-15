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
  , frame   :: Int
  , scene :: MainScene
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  , windowSize :: VecInt
  }

initialWorld :: SDL.Renderer -> ImageRsc -> VecInt -> World
initialWorld renderer' imageRsc' windowSize' = World
  { exiting = False
  , frame   = 0
  , scene = initialMainScene
  , renderer = renderer'
  , imageRsc = imageRsc'
  , windowSize = windowSize'
  }
