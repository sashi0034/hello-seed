
module MainScene.BackgroundBehavior 
( updateBackground
, renderBackground
) where
  
import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import World
import qualified SDLWrapper
import Vec
import MainScene.MainScene
import MainScene.Background



updateBackground :: MonadIO m => MainScene -> m Background
updateBackground world = do
  return background' { animCount = animCount background' + 1 }
  where 
    background' = background world



renderBackground :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderBackground r imageRsc world = do
  liftIO $ print $ animCount $ background $ scene world
  SDL.copy r (blue_bg imageRsc) (Nothing) (Just dest)


  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX size) (fromIntegral $ getY size)
    size = windowSize world
    


