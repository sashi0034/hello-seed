module MainScene.Player where
import Vec
import Types (FrameCount, LeftFrame)


data Player = Player
  { pos :: VecF
  , animCount :: Int
  , playerState :: PlayerState
  }

data PlayerState = Alive | HitStopping LeftFrame | Dead FrameCount
  deriving (Eq)


initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { pos = toVecF $ screenSize `divVec` 2
  , animCount = 0
  , playerState = Alive
  }


isAlivePlayer :: PlayerState -> Bool
isAlivePlayer Alive = True
isAlivePlayer _ = False


countAfterDiedPlayer :: PlayerState -> Int
countAfterDiedPlayer (Dead count) = count
countAfterDiedPlayer _ = 0


playerSize :: Vec Int
playerSize = Vec 24 24
