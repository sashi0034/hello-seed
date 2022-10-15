
module MainScene.Background 
( renderBackground
) where
  
import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import World
import qualified SDLWrapper
import Vec



renderBackground :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderBackground r imageRsc world = do
  SDL.copy r (blue_bg imageRsc) (Nothing) (Just dest)

  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX size) (fromIntegral $ getY size)
    size = windowSize world
    


