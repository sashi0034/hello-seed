module MainScene.Player where
import Vec
import Types (FrameCount)


data Player = Player
  { pos :: VecF
  , animCount :: Int
  , playerState :: PlayerState
  }

data PlayerState = Alive | HitStopping FrameCount | Dead 
  deriving (Eq)


initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { pos = toVecF $ screenSize `divVec` 2
  , animCount = 0
  , playerState = Alive
  }


isAlivePlayer :: PlayerState -> Bool
isAlivePlayer Dead = False
isAlivePlayer _ = True


playerSize :: Vec Int
playerSize = Vec 24 24
