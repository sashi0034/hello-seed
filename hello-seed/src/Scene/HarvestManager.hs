module Scene.HarvestManager where
import Vec
import Rendering
import Types


type ChargingCount = Int
data HarvestState = Charging ChargingCount | Ripened


data Harvest = Harvest
  { animCount :: Int
  , installedPos :: VecInt
  , currState :: HarvestState
  , whenCropped :: FrameCount
  }


maxChargingCount :: Int
maxChargingCount = 300


data HarvestManager = HarvestManager
  { harvestList :: [] Harvest }


initialHarvestManager :: VecInt -> HarvestManager
initialHarvestManager screenSize = HarvestManager
  { harvestList = harvList }
  where
    baseX = getX screenSize `div` 2
    baseY = getY screenSize `div` 2
    harvList = map (\pos -> Harvest
      { animCount=0
      , installedPos=pos
      , currState = Charging 0
      , whenCropped = -1
      }) posList
    spaceX = 24 * pixelartScale
    spaceY = 32 * pixelartScale
    numHarvX = 6
    numHarvY = 2
    posList = makePosList baseX baseY numHarvX numHarvY spaceX spaceY



makePosList baseX baseY numX numY spaceX spaceY =
  concatMap getRow yIndexes
  where
    getRow = \y -> 
      let d = abs $ y `mod` 2
          dx = d * (spaceX `div` 2)
          numX' = numX + d
      in makePosRow 
        (baseX - dx) (baseY + y * spaceY) 
        [(-numX' + d)..numX'] spaceX
    yIndexes = [-numY..numY]



makePosRow baseX y xIndexes space = map (`Vec` y) xList
  where
    xList = map (+ baseX) offset
    offset = map (* space) xIndexes


harvestSideLength :: Int
harvestSideLength = 24

harvestCellSize :: Vec Int
harvestCellSize = Vec harvestSideLength harvestSideLength
