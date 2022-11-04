

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import MainScene.MainScene
import MainScene.PlayerBehavior
import MainScene.BackgroundBehavior
import MainScene.MeteorManagerBehavior
import MainScene.InfoUIBehavior (refreshInfoUI)
import MainScene.HarvestManagerBehavior


refreshMainScene :: (MonadIO m) => World -> m MainScene
refreshMainScene world = do
  
  background' <- refreshBackground world
  player' <- refreshPlayer world
  harvestManager' <- refreshHarvestManager world
  meteorManager' <- refreshMeteorManager world
  infoUI' <- refreshInfoUI world

  return (scene world)
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , harvestManager = harvestManager'
    , infoUI = infoUI'
    }




