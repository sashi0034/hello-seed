
module MainScene.MeteorManagerBehavior where
import MainScene.MeteorManager (MeteorManager)
import MainScene.MainScene (MainScene(meteorManager))
import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import World



updateMeteorManager :: MonadIO m => MainScene -> m MeteorManager
updateMeteorManager scene = do
  return meteorManager'
  where
    meteorManager' = meteorManager scene


-- renderMeteorManager :: MonadIO m => SDL.Renderer -> ImageRsc -> World -> m ()
-- renderMeteorManager = do
  

