{-# LANGUAGE TemplateHaskell #-}
module Scene.Player where
import Vec
import Types (FrameCount, LeftFrame)
import Control.Lens (makeLenses)


type Degree = Float


data Fullness = Fullness 
  { _maxFull :: Int
  , _currFull :: Int
  }


makeLenses ''Fullness


data Player = Player
  { playerPos :: VecF
  , playerAngDeg :: Degree
  , animCount :: Int
  , playerState :: PlayerState
  , full :: Fullness
  }

data PlayerState = Normal | Pacman | HitStopping LeftFrame | Dead FrameCount
  deriving (Eq)


initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { playerPos = toVecF $ screenSize `divVec` 2
  , playerAngDeg = 0
  , animCount = 0
  , playerState = Normal
  }


isAlivePlayer :: PlayerState -> Bool
isAlivePlayer state = state == Normal || state == Pacman


countAfterDiedPlayer :: PlayerState -> Int
countAfterDiedPlayer (Dead count) = count
countAfterDiedPlayer _ = 0


playerSize :: Vec Int
playerSize = Vec 24 24
