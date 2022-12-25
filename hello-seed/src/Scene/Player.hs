{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Scene.Player where
import Vec
import Types (FrameCount, LeftFrame)
import Control.Lens (makeLenses)
import Control.Lens.Lens
import ConstParam
import Control.Lens.Getter
import CollisionUtil (ColRect (ColRect))
import AnimUtil (degToRad)


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

data PlayerState = Normal | Pacman FrameCount | HitStopping LeftFrame | Dead FrameCount
  deriving (Eq)


full :: Lens' Player Fullness
full = lens _full (\s b -> s{_full = b})


initialPlayer :: VecInt -> Player
initialPlayer screenSize = Player
  { playerPos = toVecF $ screenSize `divVec` 2
  , playerAngDeg = 0
  , animCount = 0
  , playerState = Normal
  , _full = Fullness{ _maxFull= maxPlayerFullness, _currFull=0 }
  }


isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps == Normal || isPlayerPacman ps


isPlayerPacman :: PlayerState -> Bool
isPlayerPacman ps = case ps of
  Pacman _ -> True
  _ -> False


countAfterDiedPlayer :: PlayerState -> Int
countAfterDiedPlayer (Dead count) = count
countAfterDiedPlayer _ = 0


playerSize :: Vec Int
playerSize = Vec 24 24


canBecomePacman :: Player -> Bool
canBecomePacman p =
      playerState p == Normal
  && (p ^. (full . currFull) >= p ^. (full . maxFull))


colRectPacman :: Player -> ColRect
colRectPacman p =
  let ang = degToRad $ playerAngDeg p :: Float
      offset = Vec (cos ang) (sin ang) ~* 24
      colPos = offset ~+ playerPos p
      colSize = Vec 64 (64 :: Float)
  in ColRect (colPos ~- (colSize ~* 0.5)) colSize
