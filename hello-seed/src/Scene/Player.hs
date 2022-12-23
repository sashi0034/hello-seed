{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Scene.Player where
import Vec
import Types (FrameCount, LeftFrame)
import Control.Lens (makeLenses)
import Control.Lens.Lens


type Degree = Float


data Fullness = Fullness 
  { _maxFull :: Int
  , _currFull :: Int
  }


makeLenses ''Fullness


incFullness :: Fullness -> Int -> Fullness
incFullness f v = 
  let next = v + _currFull f 
  in f { _currFull = min next $ _maxFull f }


data Player = Player
  { playerPos :: VecF
  , playerAngDeg :: Degree
  , animCount :: Int
  , playerState :: PlayerState
  , _full :: Fullness
  }

data PlayerState = Normal | Pacman | HitStopping LeftFrame | Dead FrameCount
  deriving (Eq)


full :: Lens' Player Fullness
full = lens _full (\s b -> s{_full = b})


initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { playerPos = toVecF $ screenSize `divVec` 2
  , playerAngDeg = 0
  , animCount = 0
  , playerState = Normal
  , _full = Fullness{ _maxFull=30, _currFull=0 }
  }


isAlivePlayer :: PlayerState -> Bool
isAlivePlayer state = state == Normal || state == Pacman


countAfterDiedPlayer :: PlayerState -> Int
countAfterDiedPlayer (Dead count) = count
countAfterDiedPlayer _ = 0


playerSize :: Vec Int
playerSize = Vec 24 24
