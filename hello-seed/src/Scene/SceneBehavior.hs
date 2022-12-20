

module Scene.SceneBehavior
  (refreshScene
  ) where

import Control.Monad.IO.Class
import Scene.Scene
import Scene.PlayerBehavior
import Scene.BackgroundBehavior
import Scene.MeteorManagerBehavior
import Scene.InfoUIBehavior (refreshInfoUI)
import Scene.HarvestManagerBehavior
import InputState
import qualified SDL
import qualified Scene.HarvestManager as HarvestManager
import Scene.EffectObjectBehavior (refreshEffectObjects)
import Scene.Player


refreshScene :: (MonadIO m) => Scene -> m Scene
refreshScene s = do
  let nextFrame = 1 + sceneFrame s
  let s1 = s { sceneFrame = nextFrame }
  let s' = checkShiftScene s1 $ sceneState s1
  refreshByState s' $ sceneState s'




checkShiftScene :: Scene -> SceneState -> Scene
checkShiftScene s Title =
  let butt = mouseButton $ mouse $ input $ env s
      isClicked = butt SDL.ButtonLeft
  in if isClicked
    then initPlaying $ s {sceneState = Playing}
    else s


checkShiftScene s Playing =
  let isContinued = countAfterDiedPlayer (playerState $ player s) < baseFps * 3
  in if isContinued
    then s
    else s {sceneState = Title}


calcScore :: Scene -> Int
calcScore s =
  let hl = HarvestManager.harvestList $ harvestManager s
      curr = currScore $ playingRecord s
  in foldr (\h n -> if justCropped s h then n+1 else n) curr hl




refreshByState :: (MonadIO m) => Scene -> SceneState -> m Scene

refreshByState s Title = do

  background' <- refreshBackground s
  infoUI' <- refreshInfoUI s
  effectObjects' <- refreshEffectObjects s

  return s
    { background = background'
    , infoUI = infoUI'
    , effectObjects = effectObjects'
    }

refreshByState s Playing = do

  background' <- refreshBackground s
  harvestManager' <- refreshHarvestManager s
  effectObjects' <- refreshEffectObjects s
  player' <- refreshPlayer s
  meteorManager' <- refreshMeteorManager s
  infoUI' <- refreshInfoUI s

  return s
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , harvestManager = harvestManager'
    , infoUI = infoUI'
    , effectObjects = effectObjects'
    , playingRecord = updatePlayingRecord s
    }


updatePlayingRecord :: Scene -> PlayingRecord
updatePlayingRecord s =
  let pr = playingRecord s
      newScore = calcScore s
  in pr
      { currScore = calcScore s
      , highScore = max newScore $ highScore pr}

