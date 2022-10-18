
module MainScene.MeteorManager where
import Vec (VecF)

data Meteor = Meteor
  { pos :: VecF
  , animCount :: Int }

data MeteorManager = MeteorManager
  { meteorList :: [] Meteor }


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { meteorList = [] }

