module MainScene.Background where

data Background = Background
  { animCount :: Int}


initialBackground :: Background
initialBackground = Background
  { animCount = 0
  }

