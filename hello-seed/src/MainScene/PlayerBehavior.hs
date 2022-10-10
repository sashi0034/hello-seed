
module MainScene.PlayerBehavior
( updatePlayer
, renderPlayer
) where


import World
import MainScene.Player
import MainScene.MainScene
import Vec
import qualified SDL
import qualified SDLWrapper
import Control.Monad.IO.Class
import ImageRsc
import InputIntent (InputIntent)
import InputState


movePlayer :: World -> Vec.Pos -> Player
movePlayer world deltaPos = player'{pos = Pos (x deltaPos + x currPos) (y deltaPos + y currPos)}
  where
    scene' = scene world
    player' = player scene'
    currPos = pos player'


updatePlayer :: (MonadIO m) => World -> InputState -> m Player
updatePlayer world input = do
  return  player' {pos = mousePos'}
  where
    scene' = scene world
    player' = player scene'

    mousePos' = mousePos $ mouse input




renderPlayer :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderPlayer r imageRsc world = do
  SDL.copy r (blobwob_24x24 imageRsc) (Just mask) (Just dest)

  where
    frameDuration = 200
    numFrame = 10
    cellSize = Size 24 24
    cellScale = 3
    srcX = (frame world `div` frameDuration) `mod` numFrame
    mask = fromIntegral <$> SDLWrapper.makeRect (srcX * width cellSize) 0 (width cellSize) (height cellSize)
    dest = fromIntegral <$> SDLWrapper.makeRect (x playerPos) (y playerPos) (cellScale * width cellSize) (cellScale * height cellSize)
      where
        scene' = scene world
        player' = player scene'
        playerPos = pos player'

