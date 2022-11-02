

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import MainScene.MainScene
import MainScene.PlayerBehavior
import MainScene.BackgroundBehavior
import MainScene.MeteorManagerBehavior
import MainScene.InfoUIBehavior (renderInfoUI, refreshInfoUI)


refreshMainScene :: (MonadIO m) => World -> m MainScene
refreshMainScene world = do
  
  background' <- refreshBackground world
  player' <- refreshPlayer world
  meteorManager' <- refreshMeteorManager world
  infoUI' <- refreshInfoUI world

  return (scene world)
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    , infoUI = infoUI'
    }




