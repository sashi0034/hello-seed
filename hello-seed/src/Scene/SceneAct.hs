{-# LANGUAGE RankNTypes #-}


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
import Scene.EffectObjectAct
import Scene.Player
import Data.Foldable (foldlM)
import Control.Monad
import Control.Lens
import Scene.InterAct



actUpdateScene :: Lens' Scene a -> ( Scene -> a ) -> Scene -> Scene
actUpdateScene l func s = s & l .~ func s


actUpdateSceneIO :: (MonadIO m) => Lens' Scene a -> ( Scene -> m a ) -> Scene -> m Scene
actUpdateSceneIO l func s = do
  u <- func s
  return $ s & l .~ u


playerAct :: ActorAct
playerAct = ActorAct
  (ActorUpdate $ actUpdateScene player updatePlayer)
  (ActorActive $ isSceneState Playing)
  (ActorRenderIO renderPlayer)


meteorManagerAct :: ActorAct
meteorManagerAct = ActorAct
  (ActorUpdateIO $ actUpdateSceneIO meteorManager updateMeteorManager)
  (ActorActive $ isSceneState Playing)
  (ActorRenderIO renderMeteorManager)


harvestManagerAct :: ActorAct
harvestManagerAct = ActorAct
  (ActorUpdate $ actUpdateScene harvestManager updateHarvestManager)
  (ActorActive $ isSceneState Playing)
  (ActorRenderIO renderHarvestManager)


infoUIAct :: ActorAct
infoUIAct = ActorAct
  (ActorUpdate $ actUpdateScene infoUI updateInfoUI)
  (ActorActive $ const True)
  (ActorRenderIO renderInfoUI)



backgroundAct :: ActorAct
backgroundAct = ActorAct
  (ActorUpdate $ actUpdateScene background updateBackground)
  (ActorActive (const True))
  (ActorRenderIO renderBackground)

  
setupScene :: Scene -> Scene
setupScene s =
  let acts =
        [ sceneMetaAct
        , backgroundAct
        , harvestManagerAct
        , effectObjectsAct
        , meteorManagerAct
        , playerAct
        , infoUIAct
        , interAct
        ]
  in s{ _sceneActorActList = acts }


refreshScene :: (MonadIO m) => Scene -> m Scene
refreshScene s = do
  let acts = filter (`applyActActive` s) $ s ^. actorActList

  s' <- liftIO $ foldlM (flip applyActUpdate) s acts

  liftIO $ forM_ acts (`applyActRender` s')

  return s'


checkShiftScene :: Scene -> Scene
checkShiftScene s = let meta = s^.metaInfo in
  case meta^.sceneState of
    Title ->
      let butt = mouseButton $ mouse $ input $ s^.env
          isClicked = butt SDL.ButtonLeft
      in if isClicked
        then initPlaying $ s & metaInfo .~ (meta & sceneState .~ Playing)
        else s
    Playing ->
      let isContinued = countAfterDiedPlayer (playerState $ s^.player) < baseFps * 3
      in if isContinued
        then s
        else s {_sceneMetaInfo = meta & sceneState .~ Title}


updatePlayingRecord :: Scene -> PlayingRecord
updatePlayingRecord s =
  let pr = s ^. (metaInfo . playingRecord)
  in pr
      & highScore .~ max (pr ^. currScore) (pr^.highScore)


sceneMetaAct :: ActorAct
sceneMetaAct = ActorAct
  (ActorUpdate updateSceneMeta )
  (ActorActive $ const True)
  ActorRenderNone


updateSceneMeta :: Scene -> Scene
updateSceneMeta s =
  let meta = s^.metaInfo
      nextFrame = meta^.sceneFrame + 1
      meta' = meta
        & sceneFrame .~ nextFrame
        & playingRecord .~ updatePlayingRecord s
      s1 = s & metaInfo .~ meta'
      s' = checkShiftScene s1
  in s'
