
module Scene.MeteorManager
( Meteor(..)
, MeteorManager(..)
, initialMeteorManager
, meteorCellSize
, colRectMeteor
) where
import Vec (VecF, Vec (Vec), toVecF, (~-), (~*))
import CollisionUtil
import ImageRsc (ImageGetter)

data Meteor = Meteor
  { metPos :: VecF
  , metAnimCount :: Int
  , metVelArg :: Float
  , metImage :: ImageGetter }

data MeteorManager = MeteorManager
  { metManagerElements :: [] Meteor
  , metManagerFrame :: Int
  , metManagerGenAble :: Int }


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { metManagerElements = []
  , metManagerFrame = 0
  , metManagerGenAble = 20 }


meteorCellSize :: Vec Int
meteorCellSize = Vec 16 16


colRectMeteor :: Meteor -> ColRect
colRectMeteor met = 
  ColRect (metPos met ~- (metSize ~* 0.5)) metSize
  where
    metSize = toVecF meteorCellSize
