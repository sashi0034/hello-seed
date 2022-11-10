module MainScene.Player where
import Vec


data Player = Player
  { pos :: VecF
  , animCount :: Int
  , isAlive :: Bool
  }

initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { pos = toVecF $ screenSize `divVec` 2
  , animCount = 0
  , isAlive = True
  }


playerSize :: Vec Int
playerSize = Vec 24 24
