

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import InputState
import MainScene.MainScene
import MainScene.PlayerBehavior (renderPlayer, updatePlayer)
import MainScene.BackgroundBehavior


updateMainScene :: (MonadIO  m) =>  World -> InputState -> m MainScene
updateMainScene world input = do
  let scene' = scene world
  
  background' <- updateBackground scene'
  player' <- updatePlayer world input
  return scene' 
    { player = player'
    , background = background'
    }


renderMainScene  :: (MonadIO m) => World -> m ()
renderMainScene world = do
  renderBackground r imageRsc' world
  renderPlayer r imageRsc' world

  where
    r = renderer world
    imageRsc' = imageRsc world



