

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import InputState
import MainScene.MainScene
import MainScene.PlayerBehavior (renderPlayer, updatePlayer)
import MainScene.BackgroundBehavior
import MainScene.MeteorManagerBehavior
import MainScene.InfoUIBehavior (renderInfoUI)


updateMainScene :: (MonadIO  m) =>  World -> m MainScene
updateMainScene world = do
  let scene' = scene world
  
  background' <- updateBackground scene'
  player' <- updatePlayer world (input world)
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
  renderInfoUI r (fontRsc world) (player scene') (infoUI scene')

  where
    r = renderer world
    rsc' = imageRsc world
    scene' = scene world



