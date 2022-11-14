
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
  let s = scene w
  renderPlayer (renderer w) (imageRsc w) $ player s
  return $ updatePlayer w


updatePlayer :: World -> Player
updatePlayer world = 
  let newState = updatePlayerState meteors player' $ playerState player'
  in if isHitStopping scene'
    then player'{playerState = newState}
    else player'
          { pos = calcNewPos currPos mousePos'
          , animCount = 1 + animCount player'
          , playerState = newState
          }
  where
    scene' = scene world
    player' = player scene'
    meteors = MeteorManager.meteorList $ meteorManager scene'

    currPos = pos player'
    mousePos' =  toVecF $ mousePos $ mouse $ input world


updatePlayerState :: [Meteor] -> Player -> PlayerState -> PlayerState
updatePlayerState mets p Alive = if not $ isHitWithMeteorList p mets
  then Alive
  else HitStopping 40 -- 当たった
updatePlayerState _ _ (HitStopping count) = if count > 0
  then HitStopping $ count - 1
  else Dead
updatePlayerState _ _ state = state


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

