module MainScene.HarvestManagerBehavior where
import MainScene.HarvestManager
import Control.Monad.Cont
import World
import MainScene.MainScene
import Vec
import Rendering
import ImageRsc (ImageRsc(corn_24x24))
import AnimUtil
import CollisionUtil (hitRectRect, ColRect (ColRect))
import qualified MainScene.Player as Player


refreshHarvestManager :: (MonadIO m) => World -> m HarvestManager
refreshHarvestManager w = do
  forM_ (harvestList this) (renderHarvest w)

  return $ this{
    harvestList= map (updateHarvest w) (harvestList this)
    }

  where
    this = harvestManager $ scene w


updateHarvest :: World -> Harvest -> Harvest
updateHarvest w h =
  let h' = h 
        { animCount= 1 + animCount h }
  in updateHarvestByState w h' $ currState h'


updateHarvestByState :: World -> Harvest -> HarvestState -> Harvest

updateHarvestByState _ h (Charging count) =
  let nextState = if count < maxChargingCount
        then Charging $ count + 1
        --else Charging 0
        else Ripened
  in h{ currState=nextState }

updateHarvestByState w h Ripened = 
  let p = player $ scene w
      playerSize = toVecF Player.playerSize
      thisSize = toVecF harvestCellSize
      isReaped = hitRectRect 
        (ColRect (Player.pos p ~- playerSize ~* 0.5) playerSize)
        (ColRect (toVecF (installedPos h) ~- thisSize ~* 0.5) thisSize)
      (nextState, cropped) = if isReaped 
        then (Charging 0, sceneFrame $ scene w) -- 収穫成功
        else (Ripened, whenCropped h) -- そのまま
  in h{ currState=nextState, whenCropped = cropped }


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
      r pos $ SrcRect (Vec 0 cellLen) (Vec cellLen cellLen)
      -- チャージした分だけ描画
      r' (pos ~+ posOffset) $
        SrcRect (Vec cellLen (cellLen + visibleOffsetY)) (Vec cellLen visibleH)
      where        
        rate = fromIntegral count / (fromIntegral maxChargingCount :: Float)
        visibleH = floor $ fromIntegral cellLen * rate
        visibleOffsetY = cellLen - visibleH
        halfCell = Vec (cellLen `div` 2) (cellLen `div` 2)
        posOffset = (Vec 0 visibleOffsetY ~- halfCell) ~* pixelartScale

    Ripened -> r pos $ SrcRect (Vec srcX 0) (Vec cellLen cellLen)
      where
        numFrame = 4
        frameDuration = 15
        srcX = cellLen * calcAnimFrameIndex numFrame frameDuration (animCount h)

  where
    cellLen = harvestSideLength


