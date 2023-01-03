{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Scene.PlayerAct
( updatePlayer
, renderPlayer
) where


import Scene.Scene
import Vec
import ImageRsc ( ImageRsc(blobwob_24x24) )
import InputState
import Scene.Player
import qualified Rendering
import Rendering (SrcRect(SrcRect))
import AnimUtil (calcAnimFrameIndex, valueWithEaseBegin, RangeF (RangeF))
import CollisionUtil (hitRectRect, ColRect (ColRect))
import qualified Scene.MeteorManager as MeteorManager
import Scene.MeteorManager (Meteor, MeteorManager, colRectMeteor)
import qualified SDL.Primitive
import Linear
import Control.Lens
import Ease
import ConstParam
import Control.Monad.IO.Class
import Control.Monad
import SoundRsc (SoundRsc(pacman_blink, player_damaged, game_over))
import qualified Types




updatePlayer ::
  ( HasPlayer s Player
  , HasMeteorManager s MeteorManager
  , HasEnv s Environment) => (MonadIO m) => s -> m Player
updatePlayer s =
  let p = s^.player
      meteors = MeteorManager.metManagerElements $ s^.meteorManager

      currPos = playerPos p
      mousePos' = toVecF $ mousePos $ mouse $ input $ s^.env
      (newPos, angDeg) = calcNewPosAndAngle currPos mousePos' $ playerAngDeg p

      newState = updatePlayerState meteors p

      p' = if isHitStopping s || not (isPlayerAlive newState)
        then p{playerState = newState}
        else p
              { playerPos = newPos
              , animCount = 1 + animCount p
              , playerState = newState
              , playerAngDeg = angDeg
             }

  in do
    -- 音を鳴らす
    case newState of
      (Pacman frame) -> 
        let temp = frame `mod` 90
        in when (temp == 0 || temp == 30) (playSe s pacman_blink)
      (HitStopping frame) ->
        when (frame == hitStoppingStartTime) (playSe s player_damaged)
      (Dead frame) ->
        when (frame == 0) (playSe s game_over)
      _ -> return ()

    return p'


hitStoppingStartTime :: Types.LeftFrame
hitStoppingStartTime = 40


updatePlayerState :: [Meteor] -> Player -> PlayerState
updatePlayerState mets p =
  case playerState p of
    Normal ->
      if not $ isHitWithMeteorList p mets
        then Normal
         -- 当たった
        else HitStopping hitStoppingStartTime

    Pacman fc -> if fc < ConstParam.playerPacmanTime
      then Pacman $ fc + 1
      else Normal

    (HitStopping count) -> if count > 0
      then HitStopping $ count - 1
      else Dead 0

    (Dead count) -> Dead $ 1 + count


isHitWithMeteorList :: Player -> [Meteor] -> Bool
isHitWithMeteorList p = foldr (\met hit -> hit || isHitWithMeteor p met) False


isHitWithMeteor :: Player -> Meteor -> Bool
isHitWithMeteor p met =
  hitRectRect
    (ColRect (playerPos p ~- (thisSize ~* 0.5)) thisSize)
    (colRectMeteor met)
  where
    thisSize = toVecF playerSize


calcNewPosAndAngle :: Vec Float -> Vec Float -> Degree -> (Vec Float, Degree)
calcNewPosAndAngle currPos inputPos oldAngDeg
  -- あまり動いてないならマウスの位置に
  | deltaSqr < minMovable * minMovable =
      let sqrThreshold = 2 * 2
      in  ( inputPos
          , if deltaSqr < sqrThreshold then oldAngDeg else easedAng angEasingRate)
  -- 大きく動いていたらイージングする
  | otherwise =
      (currPos ~+ (normVec ~* minMovable), easedAng angEasingRate)
  where
    deltaVec = inputPos ~- currPos
    deltaSqr = sqrMagnitude deltaVec
    angDeg = degOfVecF deltaVec
    normVec = Vec.normalize deltaVec
    minMovable = 12

    angEasingRate = 0.1
    easedAng rate
      -- 正負に関わらずきれいに度数をイージングする
      | oldAngDeg < 0 && angDeg > 0 && abs(oldAngDeg + 360 - angDeg) < abs(oldAngDeg - angDeg) =
          (oldAngDeg + 360) * (1-rate) + angDeg * rate
      | oldAngDeg > 0 && angDeg < 0 && abs(oldAngDeg - 360 - angDeg) < abs(oldAngDeg - angDeg) =
          (oldAngDeg - 360) * (1-rate) + angDeg * rate
      | otherwise =
          oldAngDeg * (1-rate) + angDeg * rate


renderPlayer ::
  ( HasPlayer s Player
  , HasEnv s Environment) => s -> IO ()
renderPlayer s =
  let
    r = renderer $ s^.env
    rsc = imageRsc $ s^.env
    p = s^.player

    frameDuration = 10
    numFrame = 10
    cellSize = playerSize

    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount p)
    src = Vec srcX 0
    dest = toVecInt $ playerPos p

  in case playerState p of
    Dead _ -> return ()

    -- パックマン 
    Pacman frame -> do
      let ang = playerAngDeg p
          halfArcBase = 45 :: Float
          animSpeed = 20
          openingMargin = 5
          halfArc = openingMargin + halfArcBase / 2 + (halfArcBase / 2) * sin (animSpeed * (pi / 180) * fromIntegral (animCount p))
          arcStart = floor $ ang + halfArc
          arcEnd =  floor $ ang - halfArc
          
          isFlashing = (frame > ConstParam.playerPacmanTime - 90) && frame `mod` 12 < 6
          yellow = V4 255 220 40 255
          (bodyColor, white) = if not isFlashing
            then (yellow, V4 255 244 220 255)
            -- パックマン終了前には点滅する
            else (V4 255 240 120 255, V4 255 244 220 64)
          orange = V4 255 196 24 255
          black = V4 80 64 48 255
          radius = floor
            $ valueWithEaseBegin (backOut (Overshoot 10)) (RangeF 12 64) 20 frame
          borderWidth = 4
      -- 白フチ
      SDL.Primitive.fillPie r (convertVecInt V2 dest) (radius + borderWidth * 2) (arcStart - borderWidth) (arcEnd + borderWidth) white
      -- 黒フチ
      SDL.Primitive.fillPie r (convertVecInt V2 dest) (radius + borderWidth) (arcStart) (arcEnd) black
      -- 中身フチ
      SDL.Primitive.fillPie r (convertVecInt V2 dest) radius (arcStart + borderWidth) (arcEnd - borderWidth) orange
      -- 中身
      SDL.Primitive.fillPie r (convertVecInt V2 dest) (radius - borderWidth) (arcStart + borderWidth) (arcEnd - borderWidth) bodyColor

    -- 普通のblobwob
    _ -> Rendering.renderPixelartCentral r (blobwob_24x24 rsc) dest $ SrcRect src cellSize


