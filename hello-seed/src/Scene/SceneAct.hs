

module Scene.SceneAct
  ( setupScene
  , refreshScene
  ) where

import Control.Monad.IO.Class
import Scene.Scene
import Scene.PlayerAct
import Scene.BackgroundAct
import Scene.MeteorManagerAct
import Scene.InfoUIAct
import Scene.HarvestManagerAct
import InputState
import qualified SDL
import qualified Scene.HarvestManager as HarvestManager
import Scene.EffectObjectAct
import Scene.Player
import Data.Foldable (foldlM)
import Control.Monad
import Control.Lens


setupScene :: Scene -> Scene
setupScene s =
  let acts =
        [ sceneMetaAct
        , backgroundAct
        , harvestManagerAct
        , effectObjectsAct
        , playerAct
        , meteorManagerAct
        , infoUIAct
        ]
  in s{ actorActList = acts }


refreshScene :: (MonadIO m) => Scene -> m Scene
refreshScene s = do
  let acts = filter (`applyActActive` s) $ actorActList s

  s' <- liftIO $ foldlM (flip applyActUpdate) s acts

  liftIO $ forM_ acts (`applyActRender` s')

  return s'


checkShiftScene :: Scene -> Scene
checkShiftScene s = let meta = sceneMeta s in
  case meta^.sceneState of
    Title ->
      let butt = mouseButton $ mouse $ input $ env s
          isClicked = butt SDL.ButtonLeft
      in if isClicked
        then initPlaying $ s {sceneMeta = meta & sceneState .~ Playing}
        else s
    Playing ->
      let isContinued = countAfterDiedPlayer (playerState $ player s) < baseFps * 3
      in if isContinued
        then s
        else s {sceneMeta = meta & sceneState .~ Title}


calcScore :: Scene -> Int
calcScore s =
  let hl = HarvestManager.harvestList $ harvestManager s
      curr = sceneMeta s ^. (playingRecord . currScore)
  in foldr (\h n -> if justCropped s h then n+1 else n) curr hl


updatePlayingRecord :: Scene -> PlayingRecord
updatePlayingRecord s =
  let pr = sceneMeta s ^. playingRecord
      newScore = calcScore s
  in pr
      & currScore .~ calcScore s
      & highScore .~ max newScore (pr^.highScore)


sceneMetaAct :: ActorAct
sceneMetaAct = ActorAct
  (ActorUpdate updateSceneMeta )
  (ActorActive $ const True)
  ActorRenderNone


updateSceneMeta :: Scene -> Scene
updateSceneMeta s =
  let meta = sceneMeta s
      nextFrame = meta^.sceneFrame + 1
      meta' = meta
        & sceneFrame .~ nextFrame
        & playingRecord .~ updatePlayingRecord s
      s1 = s { sceneMeta = meta' }
      s' = checkShiftScene s1
  in s'
