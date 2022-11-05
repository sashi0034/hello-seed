
module MainScene.PlayerBehavior
( refreshPlayer
) where


import World
import MainScene.MainScene
import Vec
import qualified SDL
import Control.Monad.IO.Class
import ImageRsc
import InputState
import MainScene.Player
import qualified Rendering
import Rendering (SrcRect(SrcRect))
import AnimUtil (calcAnimFrameIndex)


refreshPlayer :: (MonadIO m) => World -> m Player
refreshPlayer w = do
  result <- updatePlayer w
  renderPlayer (renderer w) (imageRsc w) result
  return result


movePlayer :: World -> VecF -> Player
movePlayer world deltaPos = player'{pos = deltaPos ~+ currPos}
  where
    scene' = scene world
    player' = player scene'
    currPos = pos player'


updatePlayer :: (MonadIO m) => World -> m Player
updatePlayer world = do
  return  player'
    { pos = calcNewPos currPos mousePos'
    , animCount = 1 + animCount player'
    }
  where
    scene' = scene world
    player' = player scene'

    currPos = pos player'
    mousePos' =  toVecF $ mousePos $ mouse $ input world


calcNewPos :: Vec Float -> Vec Float -> Vec Float
calcNewPos currPos inputPos
  | sqrMagnitude deltaVec < minMovable * minMovable = inputPos
  | otherwise = currPos ~+ (normVec ~* minMovable)
  where
    deltaVec = inputPos ~- currPos
    normVec = normalize deltaVec
    minMovable = 12


renderPlayer :: (MonadIO m) => SDL.Renderer -> ImageRsc -> Player -> m ()
renderPlayer r rsc player' = do
  Rendering.renderPixelartCentral r (blobwob_24x24 rsc) dest $ SrcRect src cellSize

  where
    frameDuration = 10
    numFrame = 10
    cellSize = Vec 24 (24 :: Int)

    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount player')
    src = Vec srcX 0
    dest = toVecInt $ pos player'

