{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Scene.MeteorManagerAct
( updateMeteorManager
, renderMeteorManager
) where
import Scene.MeteorManager
import Scene.Scene
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified SDL
import ImageRsc
import qualified Rendering
import Vec (toVecInt, Vec (Vec), (~+), getY, getX, VecF, VecInt)
import Rendering (SrcRect(SrcRect))
import Control.Monad
import System.Random
import AnimUtil (calcAnimFrameIndex)
import Control.Lens
import qualified Scene.Player as Player
import Scene.MeteorManager (Meteor(metImage))




type MeteorUpdate s =
  ( HasMeteorManager s MeteorManager
  , HasPlayer s Player.Player
  , HasMetaInfo s SceneMetaInfo )


updateMeteorManager :: (MeteorUpdate s, MonadIO m) => s -> m MeteorManager
updateMeteorManager s = do
  let mm = s^.meteorManager
      newFrameCount = 1 + managerFrameCount mm

  if isHitStopping s then return mm else do

    greaterMeteorList <- checkPopNewMeteor s newFrameCount (meteorList mm)

    let updatedMeteorList =
          filter (isInScreen $ s ^. (metaInfo . screenSize)) $
          map (updateMeteor s) greaterMeteorList

        mm' = mm
          { managerFrameCount = newFrameCount
          , meteorList = updatedMeteorList }

    --liftIO $ print $ length updatedMeteorList

    return mm'


updateMeteor :: MeteorUpdate s => s -> Meteor -> Meteor
updateMeteor _ meteor = meteor
  { currPos = newPos
  , animCount = newAnimCount
  }

  where
    newAnimCount = 1 + animCount meteor
    newPos = currPos meteor ~+ Vec velX velY
    velArg = velArgument meteor
    velX = cos velArg
    velY = sin velArg


calcRandomStartPos :: Int -> VecInt -> IO VecF
calcRandomStartPos pattern screenSize'
  | pattern == 0 = do
      randVal <- liftIO (randomRIO (0, screenH) :: IO Float)
      return $ Vec (-margin) randVal
  | pattern == 1 = do
      randVal <- liftIO (randomRIO (0, screenH) :: IO Float)
      return $ Vec (margin + screenW) randVal
  | pattern == 2 = do
      randVal <- liftIO (randomRIO (0, screenW) :: IO Float)
      return $ Vec randVal (-screenH)
  | pattern == 3 = do
      randVal <- liftIO (randomRIO (0, screenW) :: IO Float)
      return $ Vec randVal (margin + screenH)
  | otherwise = undefined
  where
    screenW = fromIntegral $ getX screenSize'
    screenH = fromIntegral $ getY screenSize'
    margin = 40


isInScreen ::VecInt ->  Meteor -> Bool
isInScreen screenSize' meteor =
  (-margin) < posX && posX < (screenW + margin) &&
  (-margin) < posY && posY < (screenH + margin)
  where
    margin = 60
    pos = currPos meteor
    posX = getX pos
    posY = getY pos
    screenW = fromIntegral $ getX screenSize'
    screenH = fromIntegral $ getY screenSize'


-- isOutScreen :: VecInt -> Meteor -> Bool
-- isOutScreen screenSize' meteor = not (isInScreen screenSize' meteor)


checkPopNewMeteor :: (MeteorUpdate s, MonadIO m) => s -> Int -> [] Meteor -> m ([] Meteor)
checkPopNewMeteor s count meteors

  | (count `mod` popDuration) == 0 = do
    startPosPattern <- liftIO (randomRIO (0, 3) :: IO Int)
    startPos <- liftIO $ calcRandomStartPos startPosPattern screenSize'

    randArg <- liftIO (randomRIO (-pi, pi) :: IO Float)

    let newMeteor = Meteor {
        currPos = startPos
      , animCount = 0
      , velArgument = randArg
      , metImage = octocat_16x16
      }

    return $ meteors ++ [newMeteor]

  | otherwise = return meteors

  where
    popDuration = 2
    screenSize' = s ^. (metaInfo . screenSize)


renderMeteor :: MonadIO m => SDL.Renderer -> ImageRsc -> Meteor -> m ()
renderMeteor r rsc meteor = do
  Rendering.renderPixelartCentral r (metImage meteor rsc) dest $ SrcRect src cellSize
  where
    cellSize = meteorCellSize
    dest = toVecInt $ currPos meteor
    frameDuration = 10
    numFrame = 6
    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount meteor)
    src = Vec srcX 0


renderMeteorManager :: MonadIO m => Scene -> m ()
renderMeteorManager s =
  let r = renderer $ s^.env
      rsc = imageRsc $ s^.env
      mm = s^.meteorManager

      render = renderMeteor r rsc
      meteors = meteorList mm
  in forM_ meteors render


