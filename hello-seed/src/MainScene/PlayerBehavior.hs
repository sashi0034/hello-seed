
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
import CollisionUtil (hitRectRect, ColRect (ColRect))
import qualified MainScene.MeteorManager as MeteorManager
import MainScene.MeteorManager (Meteor)


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
    , isAlive = isAlive player' && not (isHitWithMeteorList player' meteors)
    }
  where
    scene' = scene world
    player' = player scene'
    meteors = MeteorManager.meteorList $ meteorManager scene'

    currPos = pos player'
    mousePos' =  toVecF $ mousePos $ mouse $ input world


isHitWithMeteorList :: Player -> [Meteor] -> Bool
isHitWithMeteorList p = foldr (\met hit -> hit || isHitWithMeteor p met) False


isHitWithMeteor :: Player -> Meteor -> Bool
isHitWithMeteor p met =
  hitRectRect
    (ColRect (pos p ~- thisSize ~* 0.5) thisSize)
    (ColRect (MeteorManager.currPos met ~- metSize ~* 0.5) metSize)
  where
    thisSize = toVecF playerSize
    metSize = toVecF MeteorManager.meteorCellSize


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
    cellSize = playerSize

    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount player')
    src = Vec srcX 0
    dest = toVecInt $ pos player'

