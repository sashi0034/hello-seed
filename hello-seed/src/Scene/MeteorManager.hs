
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
  { currPos :: VecF
  , animCount :: Int
  , velArgument :: Float
  , metImage :: ImageGetter }

data MeteorManager = MeteorManager
  { meteorList :: [] Meteor
  , managerFrameCount :: Int }


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { meteorList = []
  , managerFrameCount = 0 }


meteorCellSize :: Vec Int
meteorCellSize = Vec 16 16


colRectMeteor :: Meteor -> ColRect
colRectMeteor met = 
  ColRect (currPos met ~- (metSize ~* 0.5)) metSize
  where
    metSize = toVecF meteorCellSize
