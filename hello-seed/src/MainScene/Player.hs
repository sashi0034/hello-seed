module MainScene.Player where
import Vec


data Player = Player
  { pos :: VecF
  , animCount :: Int
  , isAlive :: Bool
  }

initialPlayer :: Player
initialPlayer = Player
  { pos = Vec 10 10
  , animCount = 0
  , isAlive = True
  }


playerSize :: Vec Int
playerSize = Vec 24 24
