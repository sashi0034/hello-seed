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
import Scene.Player (isPlayerAlive, Player (playerState))
import Control.Lens
import Control.Monad.State


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
      updated = map (runState (updateHarvest s)) (harvestList hm)
      croppedList = 
        map (\(_, h) -> CroppedHarvest $ installedPos h) 
        $ filter fst updated
      hm' = hm 
              { harvestList= map snd updated
              , croppedStack = croppedList
              }
  in s & harvestManager .~ hm'


type Cropped = Bool


updateHarvest :: Scene -> State Harvest Cropped
updateHarvest s = do
  h <- get
  put $ h { animCount= 1 + animCount h }
  updateHarvestByState s


updateHarvestByState :: Scene -> State Harvest Cropped
updateHarvestByState s = do
  h <- get
  case currState h of
    -- チャージ中
    (Charging count) -> do
      let nextState = if count < maxChargingCount
            then Charging $ count + 1
            --else Charging 0
            else Ripened
      put h{ currState=nextState }
      return False

    -- 収穫可能
    Ripened -> do
      let p = s^.player
          playerSize = toVecF Player.playerSize
          thisSize = toVecF harvestCellSize
          isReaped = hitRectRect
            (ColRect (Player.playerPos p ~- playerSize ~* 0.5) playerSize)
            (ColRect (toVecF (installedPos h) ~- thisSize ~* 0.5) thisSize)
          (nextState, cropped) = if isReaped && (playerState p == Player.Normal)
            then (Charging 0, True) -- 収穫成功
            else (Ripened, False) -- そのまま
      put h{ currState=nextState }
      return cropped


renderHarvest :: (MonadIO m) => Scene -> Harvest -> m()
renderHarvest s h =
  let
    pos = installedPos h
    r = Rendering.renderPixelartCentral (renderer $ s^.env) (corn_24x24 $ imageRsc $ s^.env)
    r' = Rendering.renderPixelart (renderer $ s^.env) (corn_24x24 $ imageRsc $ s^.env)
  in case currState h of

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


