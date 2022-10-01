{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World
  ( World(..)
  , initialApp
  ) where
    
import qualified Vec

data World = World
  { exiting :: Bool
  , frame   :: Int
  , playerPos :: Vec.Pos
  }

initialApp :: World
initialApp = World
  { exiting = False
  , frame   = 0
  , playerPos = Vec.Pos 0 0
  }
