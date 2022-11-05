module MainScene.HarvestManagerBehavior where
import MainScene.HarvestManager
import Control.Monad.Cont
import World
import MainScene.MainScene (MainScene(harvestManager, screenSize))
import Vec
import Rendering
import ImageRsc (ImageRsc(corn_24x24))
import AnimUtil


refreshHarvestManager :: (MonadIO m) => World -> m HarvestManager
refreshHarvestManager w = do
  forM_ (harvestList this) (renderHarvest w)

  return $ this{
    harvestList= map updateHarvest (harvestList this)
    }

  where
    this = harvestManager $ scene w


updateHarvest :: Harvest -> Harvest
updateHarvest h =
  let h' = h{ animCount= 1 + animCount h }
  in updateHarvestByState h' $ currState h'


updateHarvestByState :: Harvest -> HarvestState -> Harvest
updateHarvestByState h (Charging count) =
  let nextState = if count < maxChargingCount
        then Charging $ count + 1
        --else Charging 0
        else Ripened
  in h{ currState=nextState }
updateHarvestByState h _ = h


renderHarvest :: (MonadIO m) => World -> Harvest -> m()
renderHarvest w h =
  let
    pos = installedPos h
    r = Rendering.renderPixelartCentral (renderer w) (corn_24x24 $ imageRsc w)
    r' = Rendering.renderPixelart (renderer w) (corn_24x24 $ imageRsc w)
    state = currState h
  in case state of

    Charging count -> do
      -- チャージ背景
      r pos $ SrcRect (Vec 0 cellSize) (Vec cellSize cellSize)
      -- チャージした分だけ描画
      r' (pos ~+ posOffset) $
        SrcRect (Vec cellSize (cellSize + visibleOffsetY)) (Vec cellSize visibleH)
      where        
        rate = fromIntegral count / (fromIntegral maxChargingCount :: Float)
        visibleH = floor $ fromIntegral cellSize * rate
        visibleOffsetY = cellSize - visibleH
        halfCell = Vec (cellSize `div` 2) (cellSize `div` 2)
        posOffset = (Vec 0 visibleOffsetY ~- halfCell) ~* pixelartScale

    Ripened -> r pos $ SrcRect (Vec srcX 0) (Vec cellSize cellSize)
      where
        numFrame = 4
        frameDuration = 15
        srcX = cellSize * calcAnimFrameIndex numFrame frameDuration (animCount h)

  where
    cellSize = 24 :: Int


