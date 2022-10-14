
module MainScene.Player
( updatePlayer
, renderPlayer
) where


import World
import MainScene.MainScene
import Vec
import qualified SDL
import qualified SDLWrapper
import Control.Monad.IO.Class
import ImageRsc
import InputIntent (InputIntent)
import InputState


movePlayer :: World -> VecF -> Player
movePlayer world deltaPos = player'{pos = deltaPos ~+ currPos}
  where
    scene' = scene world
    player' = player scene'
    currPos = pos player'




updatePlayer :: (MonadIO m) => World -> InputState -> m Player
updatePlayer world input = do
  return  player' {pos = toVecF mousePos'}
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
    cellSize = Vec 24 (24 :: Int)
    cellScale = 3
    srcX = (frame world `div` frameDuration) `mod` numFrame
    mask = fromIntegral <$> SDLWrapper.makeRect (srcX * getX cellSize) 0 (getX cellSize) (getY cellSize)
    dest = fromIntegral <$> SDLWrapper.makeRect
      (-(getX cellSize `div` 2) * cellScale + floor (getX playerPos))
      (-(getY cellSize `div` 2) * cellScale + floor (getY playerPos))
      (cellScale * getX cellSize)
      (cellScale * getY cellSize)

      where
        scene' = scene world
        player' = player scene'
        playerPos = pos player'

