

module MainScene.MainSceneBehavior
  (refreshMainScene
  ) where

import Control.Monad.IO.Class
import World
import MainScene.MainScene
import MainScene.PlayerBehavior
import MainScene.BackgroundBehavior
import MainScene.MeteorManagerBehavior
import MainScene.InfoUIBehavior (refreshInfoUI)
import MainScene.HarvestManagerBehavior
import InputState
import qualified SDL
import qualified MainScene.HarvestManager as HarvestManager
import MainScene.EffectObjectBehavior (refreshEffectObjects)
import MainScene.Player


refreshMainScene :: (MonadIO m) => World -> m MainScene
refreshMainScene w = do
  let nextFrame = 1 + sceneFrame (scene w)
  let w' = w { scene=(checkShiftScene w $ sceneState $ scene w) { sceneFrame = nextFrame } }
  refreshByState w' $ sceneState $ scene w'




checkShiftScene :: World -> SceneState -> MainScene
checkShiftScene w Title =
  let s = scene w
      butt = mouseButton $ mouse $ input w
      isClicked = butt SDL.ButtonLeft
  in if isClicked
    then initPlaying $ s {sceneState = Playing}
    else s


checkShiftScene w Playing =
  let s = scene w
      isContinued = countAfterDiedPlayer (playerState $ player s) < baseFps * 3
  in if isContinued
    then s
    else s {sceneState = Title}


calcScore :: MainScene -> Int
calcScore ms =
  let hl = HarvestManager.harvestList $ harvestManager ms
      curr = currScore $ playingRecord ms
  in foldr (\h n -> if justCropped ms h then n+1 else n) curr hl




refreshByState :: (MonadIO m) => World -> SceneState -> m MainScene

refreshByState w Title = do

  background' <- refreshBackground w
  infoUI' <- refreshInfoUI w
  effectObjects' <- refreshEffectObjects w

  return (scene w)
    { background = background'
    , infoUI = infoUI'
    , effectObjects = effectObjects'
    }

refreshByState w Playing = do
  let ms = scene w

  background' <- refreshBackground w
  harvestManager' <- refreshHarvestManager w
  effectObjects' <- refreshEffectObjects w
  player' <- refreshPlayer w
  meteorManager' <- refreshMeteorManager w
  infoUI' <- refreshInfoUI w

  return ms
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , harvestManager = harvestManager'
    , infoUI = infoUI'
    , effectObjects = effectObjects'
    , playingRecord = updatePlayingRecord ms
    }


updatePlayingRecord :: MainScene -> PlayingRecord
updatePlayingRecord ms =
  let pr = playingRecord ms
      newScore = calcScore ms
  in pr
      { currScore = calcScore ms
      , highScore = max newScore $ highScore pr}

