module MainScene.Player where

import Vec

data Player = Player
  { pos :: VecF
  , animCount :: Int
  }


initialPlayer :: Player
initialPlayer = Player
  { pos = Vec 10 10
  , animCount = 0
  }

