
module MainScene.MeteorManager where
import Vec (VecF)

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

