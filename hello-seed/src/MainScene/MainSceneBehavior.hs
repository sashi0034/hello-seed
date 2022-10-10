

module MainScene.MainSceneBehavior where

import Control.Monad.IO.Class
import World
import InputState
import MainScene.MainScene
import qualified SDL
import ImageRsc
import MainScene.PlayerBehavior (renderPlayer, updatePlayer)


updateMainScene :: (MonadIO  m) =>  World -> InputState -> m MainScene
updateMainScene world input = do
  let scene' = scene world
  
  player' <- updatePlayer world input
  return scene' { player = player'}


renderMainScene  :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderMainScene r imageRsc world = do
  renderPlayer r imageRsc world


