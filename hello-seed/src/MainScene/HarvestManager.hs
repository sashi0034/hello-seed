module MainScene.HarvestManager where
import Vec
import Rendering


data Harvest = Harvest
  { animCount :: Int
  , installedPos :: VecInt
  }


data HarvestManager = HarvestManager
  { harvestList :: [] Harvest }


initialHarvestManager :: VecInt -> HarvestManager
initialHarvestManager screenSize = HarvestManager
  { harvestList = harvList }
  where
    baseX = getX screenSize `div` 2
    baseY = getY screenSize `div` 2
    harvList = map (\pos -> Harvest{animCount=0, installedPos=pos}) posList
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
