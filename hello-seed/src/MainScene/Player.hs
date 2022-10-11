module MainScene.Player where

import Vector

data Player = Player
  { pos :: Pos
  , animCount :: Int
  }


initialPlayer :: Player
initialPlayer = Player
  { pos = Pos 10 10
  , animCount = 0
  }
