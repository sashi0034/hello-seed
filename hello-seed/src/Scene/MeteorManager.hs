
module Scene.MeteorManager where
import Vec (VecF, Vec (Vec))

data Meteor = Meteor
  { currPos :: VecF
  , isDead :: Bool
  , animCount :: Int
  , velArgument :: Float }

data MeteorManager = MeteorManager
  { meteorList :: [] Meteor
  , managerFrameCount :: Int }


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { meteorList = []
  , managerFrameCount = 0 }


meteorCellSize :: Vec Int
meteorCellSize = Vec 16 16
