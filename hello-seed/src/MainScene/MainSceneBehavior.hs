

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
import qualified MainScene.Player as Player
import qualified SDL
import qualified MainScene.HarvestManager as HarvestManager


refreshMainScene :: (MonadIO m) => World -> m MainScene
refreshMainScene w = do
  let w' = w { scene=checkShiftScene w $ sceneState $ scene w }
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
      isPlayerAlive = Player.isAlive $ player s
  in if isPlayerAlive
    then s
    else s {sceneState = Title}


calcScore :: MainScene -> Int
calcScore ms = 
  let hl = HarvestManager.harvestList $ harvestManager ms
      curr = currScore $ playingRecord ms
  in foldr (\h n -> if HarvestManager.justCropped h then n+1 else n) curr hl




refreshByState :: (MonadIO m) => World -> SceneState -> m MainScene

refreshByState w Title = do
  
  background' <- refreshBackground w
  infoUI' <- refreshInfoUI w

  return (scene w)
    { background = background'
    , infoUI = infoUI'
    }

refreshByState w Playing = do
  let ms = scene w

  background' <- refreshBackground w
  harvestManager' <- refreshHarvestManager w
  player' <- refreshPlayer w
  meteorManager' <- refreshMeteorManager w
  infoUI' <- refreshInfoUI w

  return ms
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , harvestManager = harvestManager'
    , infoUI = infoUI'
    , playingRecord = updatePlayingRecord ms
    }


updatePlayingRecord :: MainScene -> PlayingRecord
updatePlayingRecord ms = 
  let pr = playingRecord ms
      newScore = calcScore ms
  in pr
      { currScore = calcScore ms
      , highScore = max newScore $ highScore pr}

