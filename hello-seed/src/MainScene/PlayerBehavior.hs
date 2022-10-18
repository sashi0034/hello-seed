
module MainScene.PlayerBehavior
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
import InputState
import MainScene.Player
import qualified Rendering
import Rendering (SrcRect(SrcRect))


movePlayer :: World -> VecF -> Player
movePlayer world deltaPos = player'{pos = deltaPos ~+ currPos}
  where
    scene' = scene world
    player' = player scene'
    currPos = pos player'


updatePlayer :: (MonadIO m) => World -> InputState -> m Player
updatePlayer world input = do
  return  player' 
    { pos = calcNewPos currPos mousePos'
    , animCount = 1 + animCount player'
    }
  where
    scene' = scene world
    player' = player scene'

    currPos = pos player'
    mousePos' =  toVecF $ mousePos $ mouse input


calcNewPos :: Vec Float -> Vec Float -> Vec Float
calcNewPos currPos inputPos 
  | sqrMagnitude deltaVec < minMovable * minMovable = inputPos
  | otherwise = currPos ~+ (normVec ~* minMovable)
  where 
    deltaVec = inputPos ~- currPos
    normVec = normalize deltaVec
    minMovable = 12


renderPlayer :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderPlayer r imageRsc world = do
  --liftIO $ print $ animCount'
  Rendering.renderPixelartCentral r (blobwob_24x24 imageRsc) dest $ SrcRect src cellSize

  where
    frameDuration = 10
    numFrame = 10
    cellSize = Vec 24 (24 :: Int)

    animCount' = animCount player'
    srcX = (animCount' `div` frameDuration) `mod` numFrame
    src = Vec (srcX * getX cellSize) 0
    dest = toVecInt playerPos

    scene' = scene world
    player' = player scene'
    playerPos = pos player'

