

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import InputState
import MainScene.MainScene
import MainScene.PlayerBehavior (renderPlayer, updatePlayer)
import MainScene.BackgroundBehavior
import MainScene.MeteorManagerBehavior


updateMainScene :: (MonadIO  m) =>  World -> InputState -> m MainScene
updateMainScene world input = do
  let scene' = scene world
  
  background' <- updateBackground scene'
  player' <- updatePlayer world input
  meteorManager' <- updateMeteorManager scene'

  return scene' 
    { player = player'
    , background = background'
    , meteorManager = meteorManager'
    }


renderMainScene  :: (MonadIO m) => World -> m ()
renderMainScene world = do
  renderBackground r rsc' world
  renderPlayer r rsc' $ player scene'
  renderMeteorManager r rsc' $ meteorManager scene'

  where
    r = renderer world
    rsc' = imageRsc world
    scene' = scene world



