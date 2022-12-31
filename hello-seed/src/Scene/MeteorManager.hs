
module Scene.MeteorManager
( Meteor(..)
, MeteorManager(..)
, initialMeteorManager
, colRectMeteor
, getMetGenAbleNext
, metCellSize
, metImage
, MeteorGrade(..)
) where
import Vec (VecF, Vec (Vec), toVecF, (~-), (~*), VecInt)
import CollisionUtil
import ImageRsc (ImageGetter, ImageRsc (octocat_16x16, gorilla_24x24))


data MeteorGrade = 
    MeteorGradeNormal 
  | MeteorGradeStrong 
  deriving (Eq)


data Meteor = Meteor
  { metPos :: VecF
  , metAnimCount :: Int
  , metVel :: VecF
  , metGrade :: MeteorGrade }

data MeteorManager = MeteorManager
  { metManagerElements :: [] Meteor
  , metManagerFrame :: Int
  , metManagerGenAble :: Int }


metImage :: Meteor -> ImageGetter
metImage met = case metGrade met of
  MeteorGradeNormal -> octocat_16x16
  MeteorGradeStrong -> gorilla_24x24

metCellSize :: Meteor -> VecInt
metCellSize met = case metGrade met of
  MeteorGradeNormal -> Vec 16 16
  MeteorGradeStrong -> Vec 24 24


initialMeteorManager :: MeteorManager
initialMeteorManager = MeteorManager 
  { metManagerElements = []
  , metManagerFrame = 0
  , metManagerGenAble = 15 }


getMetGenAbleNext :: Int
getMetGenAbleNext = 10


colRectMeteor :: Meteor -> ColRect
colRectMeteor met = 
  ColRect (metPos met ~- (metSize ~* 0.5)) metSize
  where
    metSize = toVecF $ metCellSize met
