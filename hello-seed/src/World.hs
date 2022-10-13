{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World
  ( World(..)
  , initialWorld
  ) where
    
import Vec
import MainScene.MainScene

data World = World
  { exiting :: Bool
  , frame   :: Int
  , scene :: MainScene
  }

initialWorld :: World
initialWorld = World
  { exiting = False
  , frame   = 0
  , scene = initialMainScene
  }
