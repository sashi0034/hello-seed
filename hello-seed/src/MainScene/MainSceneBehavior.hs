

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
import qualified SDL as SDL.Input.Mouse


refreshMainScene :: (MonadIO m) => World -> m MainScene
refreshMainScene w = do
  let w' = w { scene=checkShiftScene w $ sceneState $ scene w }
  refreshByState w' $ sceneState $ scene w'




checkShiftScene :: World -> SceneState -> MainScene
checkShiftScene w Title = 
  let s = scene w
      butt = mouseButton $ mouse $ input w
      isClicked = butt SDL.Input.Mouse.ButtonLeft
  in if isClicked 
    then s {sceneState = Playing}
    else s


checkShiftScene w Playing =
  scene w




refreshByState :: (MonadIO m) => World -> SceneState -> m MainScene

refreshByState w Title = do
  
  background' <- refreshBackground w
  infoUI' <- refreshInfoUI w

  return (scene w)
    { background = background'
    , infoUI = infoUI'
    }

refreshByState w Playing = do
  
  background' <- refreshBackground w
  harvestManager' <- refreshHarvestManager w
  player' <- refreshPlayer w
  meteorManager' <- refreshMeteorManager w
  infoUI' <- refreshInfoUI w

  return (scene w)
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , harvestManager = harvestManager'
    , infoUI = infoUI'
    }




