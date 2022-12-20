
module Scene.PlayerBehavior
( refreshPlayer
) where


import Scene.Scene
import Vec
import qualified SDL
import Control.Monad.IO.Class
import ImageRsc ( ImageRsc(blobwob_24x24) )
import InputState
import Scene.Player
import qualified Rendering
import Rendering (SrcRect(SrcRect))
import AnimUtil (calcAnimFrameIndex)
import CollisionUtil (hitRectRect, ColRect (ColRect))
import qualified Scene.MeteorManager as MeteorManager
import Scene.MeteorManager (Meteor)


refreshPlayer :: (MonadIO m) => Scene -> m Player
refreshPlayer s = do
  renderPlayer (renderer $ env s) (imageRsc $ env s) $ player s
  return $ updatePlayer s


updatePlayer :: Scene -> Player
updatePlayer s =
  let newState = updatePlayerState meteors player' $ playerState player'
  in if isHitStopping s || not (isAlivePlayer newState)
    then player'{playerState = newState}
    else player'
          { pos = calcNewPos currPos mousePos'
          , animCount = 1 + animCount player'
          , playerState = newState
          }
  where
    player' = player s
    meteors = MeteorManager.meteorList $ meteorManager s

    currPos = pos player'
    mousePos' =  toVecF $ mousePos $ mouse $ input $ env s


updatePlayerState :: [Meteor] -> Player -> PlayerState -> PlayerState
updatePlayerState mets p Alive = if not $ isHitWithMeteorList p mets
  then Alive
  else HitStopping 40 -- 当たった
updatePlayerState _ _ (HitStopping count) = if count > 0
  then HitStopping $ count - 1
  else Dead 0
updatePlayerState _ _ (Dead count) = Dead $ 1 + count
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
renderPlayer r rsc player' = 
  case playerState player' of
    Dead _ -> return ()
    _ -> Rendering.renderPixelartCentral r (blobwob_24x24 rsc) dest $ SrcRect src cellSize

  where
    frameDuration = 10
    numFrame = 10
    cellSize = playerSize

    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount player')
    src = Vec srcX 0
    dest = toVecInt $ pos player'

