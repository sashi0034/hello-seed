module Scene.HarvestManagerAct (harvestManagerAct) where
import Scene.HarvestManager
import Control.Monad.Cont
import Scene.Scene
import Vec
import Rendering
import ImageRsc (ImageRsc(corn_24x24))
import AnimUtil
import CollisionUtil (hitRectRect, ColRect (ColRect))
import qualified Scene.Player as Player
import Scene.Player (isAlivePlayer, Player (playerState))
import Control.Lens


harvestManagerAct :: ActorAct
harvestManagerAct = ActorAct
  (ActorUpdate updateHarvestManager)
  (ActorActive $ isSceneState Playing)
  (ActorRenderIO renderHarvestManager)


renderHarvestManager :: Scene -> IO ()
renderHarvestManager s = do
  forM_ (harvestList hm) (renderHarvest s)
  where
    hm = s ^. harvestManager


updateHarvestManager :: Scene -> Scene
updateHarvestManager s =
  let hm = s^.harvestManager
      hm' = hm {harvestList= map (updateHarvest s) (harvestList hm)}
  in s & harvestManager .~ hm'


updateHarvest :: Scene -> Harvest -> Harvest
updateHarvest w h =
  let h' = h
        { animCount= 1 + animCount h }
  in updateHarvestByState w h' $ currState h'


updateHarvestByState :: Scene -> Harvest -> HarvestState -> Harvest

updateHarvestByState _ h (Charging count) =
  let nextState = if count < maxChargingCount
        then Charging $ count + 1
        --else Charging 0
        else Ripened
  in h{ currState=nextState }

updateHarvestByState s h Ripened =
  let p = s^.player
      playerSize = toVecF Player.playerSize
      thisSize = toVecF harvestCellSize
      isReaped = hitRectRect
        (ColRect (Player.playerPos p ~- playerSize ~* 0.5) playerSize)
        (ColRect (toVecF (installedPos h) ~- thisSize ~* 0.5) thisSize)
      (nextState, cropped) = if isReaped && isAlivePlayer (playerState p)
        then (Charging 0, s ^. (sceneMeta . sceneFrame)) -- 収穫成功
        else (Ripened, whenCropped h) -- そのまま
  in h{ currState=nextState, whenCropped = cropped }


renderHarvest :: (MonadIO m) => Scene -> Harvest -> m()
renderHarvest s h =
  let
    pos = installedPos h
    r = Rendering.renderPixelartCentral (renderer $ s^.env) (corn_24x24 $ imageRsc $ s^.env)
    r' = Rendering.renderPixelart (renderer $ s^.env) (corn_24x24 $ imageRsc $ s^.env)
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


