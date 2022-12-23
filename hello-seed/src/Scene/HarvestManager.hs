{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Scene.HarvestManager where
import Vec
import Rendering


type ChargingCount = Int
data HarvestState = Charging ChargingCount | Ripened


data CroppedHarvest = CroppedHarvest VecInt


data Harvest = Harvest
  { animCount :: Int
  , installedPos :: VecInt
  , currState :: HarvestState
  }


maxChargingCount :: Int
maxChargingCount = 300


data HarvestManager = HarvestManager
  { harvestList :: [] Harvest
  , croppedStack :: [] CroppedHarvest }


initialHarvestManager :: VecInt -> HarvestManager
initialHarvestManager screenSize = HarvestManager
  { harvestList = harvList
  , croppedStack = [] }
  where
    baseX = getX screenSize `div` 2
    baseY = getY screenSize `div` 2
    harvList = map (\pos -> Harvest
      { animCount=0
      , installedPos=pos
      , currState = Charging 0
      }) posList
    spaceX = 24 * pixelartScale
    spaceY = 32 * pixelartScale
    numHarvX = 6
    numHarvY = 2
    posList = makePosList baseX baseY numHarvX numHarvY spaceX spaceY


makePosList :: Integral a => a -> a -> a -> a -> a -> a -> [Vec a]
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
