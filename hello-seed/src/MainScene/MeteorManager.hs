
module MainScene.MeteorManager where
import Vec (VecF)

data Meteor = Meteor
  { currPos :: VecF
  , animCount :: Int }

data MeteorManager = MeteorManager
  { meteorList :: [] Meteor
  , managerFrameCount :: Int }


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { meteorList = []
  , managerFrameCount = 0 }

