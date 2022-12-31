{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Scene.HarvestManager where
import Vec
import Rendering
import ConstParam
import Control.Monad.State
import AnimUtil (degToRad)


type ChargingCount = Int
data HarvestState = Charging ChargingCount | Ripened


data CroppedHarvest = CroppedHarvest VecInt


data Harvest = Harvest
  { animCount :: Int
  , harvestPos :: VecInt
  , currState :: HarvestState
  }


maxChargingCount :: Int
maxChargingCount = maxHarvestChargingCount


data HarvestManager = HarvestManager
  { harvestList :: [] Harvest
  , croppedStack :: [] CroppedHarvest }


initialHarvestManager :: VecInt -> HarvestManager
initialHarvestManager screenSize = HarvestManager
  { harvestList = harvList
  , croppedStack = [] }
  where
    harvList = makeHarvestList ConstParam.initialLevel screenSize


makeHarvestList :: Int -> Vec Int -> [Harvest]
makeHarvestList level screenSize = map (\pos -> Harvest
    { animCount=0
    , harvestPos=pos
    , currState = Charging 0
    })
    $ execState (makeHarvestPosList level baseX baseY) []
  where
    baseX = getX screenSize `div` 2
    baseY = getY screenSize `div` 2


makeHarvestPosList :: Int -> Int -> Int -> State [Vec Int] ()
makeHarvestPosList level baseX baseY =
  let addPos = \v -> modify (++ [Vec (baseX + getX v) (baseY + getY v)])
  in case level `mod` 4 of
    1 -> do
      -- 端
      let (numX, numY) = (5, 2)
          (Vec spaceX spaceY) = Vec 24 32 ~* pixelartScale
      forM_ [-numY..numY] $ \y -> do
        let d = abs $ y `mod` 2
            dx = -d * (spaceX `div` 2)
            side = 2
        forM_ [-numX .. numX + d] $ \x -> do
          if y == 0 && abs x < 2
            -- 中心にくぼみをつける
            then return ()
            else addPos $ Vec (dx + x * spaceX) (y *spaceY)

    2 -> do
      -- 端
      let (numX, numY) = (6, 2)
          (Vec spaceX spaceY) = Vec 24 32 ~* pixelartScale
      forM_ [-numY..numY] $ \y -> do
        let d = abs $ y `mod` 2
            dx = -d * (spaceX `div` 2)
            side = 2
        forM_ ([-numX .. -numX + side] ++ [numX + d - side .. numX + d]) $ \x -> do
          addPos $ Vec (dx + x * spaceX) (y *spaceY)
      -- 中心に円形
      forM_ [(6 :: Int, 96), (12, 192)] $ \(numRot, radius) -> do
        forM_ [r * (360 `div` numRot) | r <- [-numRot .. (numRot + 1)]] $ \deg -> do
          addPos $ toVecInt $ vecFromDeg deg ~* radius

    3 -> do
      -- 波線
      let (numX, numY) = (6, 1)
          (Vec spaceX spaceY) = Vec 24 48 ~* pixelartScale
      forM_ [-numY..numY] $ \y -> do
        forM_ [-numX .. numX] $ \x -> do
          let phase = fromIntegral $ x * (180 `div` numX) :: Float
              amp = fromIntegral spaceY * sin (degToRad phase)
          addPos $ Vec
            (x * spaceX)
            (y * spaceY + floor amp)

    0 -> do
      let space = 24 * pixelartScale
          -- 正方形を置く
          putSq = \len x y -> forM_ [-len..(len - 1)] $ \i -> do
            let offset = Vec x y
            addPos $ offset ~+ Vec (i * space) (-len * space)
            addPos $ offset ~+ Vec (len * space) (i * space)
            addPos $ offset ~+ Vec (-i * space) (len * space)
            addPos $ offset ~+ Vec (-len * space) (-i * space)
          (groupX, groupY) = (368, 144)
      putSq 2 0 0
      putSq 1 (-groupX) (-groupY)
      putSq 1 groupX groupY
      putSq 1 groupX (-groupY)
      putSq 1 (-groupX) groupY

    _ -> undefined


harvestSideLength :: Int
harvestSideLength = 24

harvestCellSize :: Vec Int
harvestCellSize = Vec harvestSideLength harvestSideLength
