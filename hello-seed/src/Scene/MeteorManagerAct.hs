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
import Vec (toVecInt, Vec (Vec), (~+), getY, getX, VecF, VecInt, vecZero, vecFromRad, radOfVecF, (~-), (~*), normalize, sqrMagnitude)
import Rendering (SrcRect(SrcRect))
import Control.Monad
import System.Random
import AnimUtil (calcAnimFrameIndex)
import Control.Lens
import qualified Scene.Player as Player
import Scene.MeteorManager
import Control.Monad.State
import Data.List (partition)
import Scene.Player




type MeteorUpdate s =
  ( HasMeteorManager s MeteorManager
  , HasPlayer s Player.Player
  , HasMetaInfo s SceneMetaInfo )


updateMeteorManager :: (MeteorUpdate s, MonadIO m) => s -> m MeteorManager
updateMeteorManager s = do
  let mm = s^.meteorManager
      newFrameCount = 1 + metManagerFrame mm
      genAble = metManagerGenAble mm

  if isHitStopping s then return mm else do

    newMeteorList <- checkPopNewMeteor s newFrameCount genAble

    updatedMeteorList <-
      checkUpgradeMeteorList s <$>
      mapM (updateMeteor s) 
      (metManagerElements mm ++ newMeteorList)

    let mm' = mm
          { metManagerFrame = newFrameCount
          , metManagerElements = updatedMeteorList
          , metManagerGenAble = genAble - length newMeteorList }

    --liftIO $ print $ length updatedMeteorList

    return mm'


-- 数が少なくなってきたら個体のいくつかをパワーアップさせる
checkUpgradeMeteorList :: (MeteorUpdate s) => s -> [Meteor] -> [Meteor]
checkUpgradeMeteorList s met =
  let mm = s^.meteorManager
      lv = s^.metaInfo^.playingRecord^.currLevel
      len = length met
  in if metManagerGenAble mm > 0 then met else case lv of
    1 -> if len < 5 then upgradeMeteorList 1 met else met
    2 -> if len < 10 then upgradeMeteorList 2 met else met
    3 -> if len < 40 then upgradeMeteorList 4 met
         else if len < 20 then upgradeMeteorList 2 met
         else upgradeMeteorList 1 met
    4 -> if len < 40 then upgradeMeteorList 5 met
         else if len < 20 then upgradeMeteorList 3 met
         else upgradeMeteorList 1 met
    _ -> if len < 40 then upgradeMeteorList 7 met
         else if len < 20 then upgradeMeteorList 5 met
         else if len < 10 then upgradeMeteorList 3 met
         else upgradeMeteorList 1 met


upgradeMeteorList :: Int -> [Meteor] -> [Meteor]
upgradeMeteorList numUpgrade list =
  let (upgraded, normals) = partition (\met -> metGrade met == MeteorGradeStrong) list
      numNeedUpgrade = numUpgrade - length upgraded
  in if numNeedUpgrade <= 0
    then list
    else
      let (upgrading, nonUpgrade) = splitAt numNeedUpgrade normals
      in upgraded
        ++ map (\met -> met { metGrade = MeteorGradeStrong
                            } ) upgrading
        ++ nonUpgrade


updateMeteor :: (MeteorUpdate s, MonadIO m) => s -> Meteor -> m Meteor
updateMeteor s meteor =
  let newAnimCount = 1 + metAnimCount meteor
      newPos = metPos meteor ~+ metVel meteor
      newMet = meteor
        { metPos = newPos
        , metAnimCount = newAnimCount
        }
  in case metGrade meteor of
    MeteorGradeNormal ->
      if isInScreen (s ^. (metaInfo . screenSize)) newMet
        then return newMet
        else locateMeteorRandom s meteor
    MeteorGradeStrong ->
      let speed = 0.1
          maxVel = 2
          newVel = metVel newMet
            ~+ vecFromRad (radOfVecF (playerPos (s^.player) ~- newPos)) ~* speed
          newVel' = if sqrMagnitude newVel < maxVel * maxVel
            then newVel
            else normalize newVel ~* maxVel
      in return newMet { metVel = newVel' }


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
    pos = metPos meteor
    posX = getX pos
    posY = getY pos
    screenW = fromIntegral $ getX screenSize'
    screenH = fromIntegral $ getY screenSize'


-- isOutScreen :: VecInt -> Meteor -> Bool
-- isOutScreen screenSize' meteor = not (isInScreen screenSize' meteor)


checkPopNewMeteor :: (MeteorUpdate s, MonadIO m) => s -> Int -> Int -> m ([] Meteor)
checkPopNewMeteor s count genAble
  | (count `mod` popDuration) == 0 = do
    newMeteor <- locateMeteorRandom s Meteor
      { metPos = vecZero
      , metAnimCount = 0
      , metVel = vecZero
      , metGrade = MeteorGradeNormal
      }

    return $ take genAble [newMeteor]

  | otherwise = return []

  where
    popDuration = 2


locateMeteorRandom :: (MeteorUpdate s, MonadIO m) => s -> Meteor -> m Meteor
locateMeteorRandom s met = do
  startPosPattern <- liftIO (randomRIO (0, 3) :: IO Int)
  startPos <- liftIO $ calcRandomStartPos startPosPattern screenSize'

  randArg <- liftIO (randomRIO (-pi, pi) :: IO Float)
  return met
    { metPos = startPos
    , metVel = vecFromRad randArg ~* speed
    }
  where
    screenSize' = s ^. (metaInfo . screenSize)
    lv = s^.metaInfo^.playingRecord^.currLevel
    speed = 1.0 + (fromIntegral lv - 1) / 10

renderMeteor :: MonadIO m => SDL.Renderer -> ImageRsc -> Meteor -> m ()
renderMeteor r rsc meteor = do
  Rendering.renderPixelartCentral r (metImage meteor rsc) dest $ SrcRect src cellSize
  where
    cellSize = metCellSize meteor
    dest = toVecInt $ metPos meteor
    frameDuration = 10
    numFrame = case metGrade meteor of
      MeteorGradeNormal -> 6
      MeteorGradeStrong -> 2
    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (metAnimCount meteor)
    src = Vec srcX 0


renderMeteorManager :: MonadIO m => Scene -> m ()
renderMeteorManager s =
  let r = renderer $ s^.env
      rsc = imageRsc $ s^.env
      mm = s^.meteorManager

      render = renderMeteor r rsc
      meteors = metManagerElements mm
  in forM_ meteors render


