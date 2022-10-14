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

data World = World
  { exiting :: Bool
  , frame   :: Int
  , scene :: MainScene
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  }

initialWorld :: SDL.Renderer -> ImageRsc -> World
initialWorld renderer' imageRsc' = World
  { exiting = False
  , frame   = 0
  , scene = initialMainScene
  , renderer = renderer'
  , imageRsc = imageRsc'
  }
