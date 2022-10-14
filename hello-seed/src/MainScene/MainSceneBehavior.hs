

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import InputState
import MainScene.MainScene
import qualified SDL
import ImageRsc
import MainScene.Player (renderPlayer, updatePlayer)
import MainScene.Background (renderBackground)


updateMainScene :: (MonadIO  m) =>  World -> InputState -> m MainScene
updateMainScene world input = do
  let scene' = scene world
  
  player' <- updatePlayer world input
  return scene' { player = player'}


renderMainScene  :: (MonadIO m) => World -> m ()
renderMainScene world = do
  renderBackground r imageRsc' world
  renderPlayer r imageRsc' world

  where
    r = renderer world
    imageRsc' = imageRsc world



