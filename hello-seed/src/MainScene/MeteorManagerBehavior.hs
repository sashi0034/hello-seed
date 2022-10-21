
module MainScene.MeteorManagerBehavior where
import MainScene.MeteorManager
import MainScene.MainScene (MainScene(meteorManager, screenSize))
import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import qualified Rendering
import Vec (toVecInt, Vec (Vec), (~+), getY, getX, VecF, VecInt)
import Rendering (SrcRect(SrcRect))
import Control.Monad
import System.Random
import AnimUtil (calcAnimFrameIndex)



updateMeteor :: MainScene -> Meteor -> Meteor
updateMeteor scene meteor = meteor
  { currPos = newPos
  , animCount = newAnimCount
  }
  -- TODO: dead meteorを取り除く
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
    margin = 20


checkPopNewMeteor :: MonadIO m => MainScene -> Int -> [] Meteor -> m ([] Meteor)
checkPopNewMeteor scene frameCount meteors 

  | (frameCount `mod` popDuration) == 0 = do
    startPosPattern <- liftIO (randomRIO (0, 3) :: IO Int)
    startPos <- liftIO $ calcRandomStartPos startPosPattern screenSize'

    randArg <- liftIO (randomRIO (-pi, pi) :: IO Float)
    
    let newMeteor = Meteor { 
        currPos = startPos
      , isDead = False
      , animCount=0
      , velArgument = randArg
      }

    return $ meteors ++ [newMeteor]

  | otherwise = return meteors

  where 
    popDuration = 30
    screenSize' = screenSize scene
    


updateMeteorManager :: MonadIO m => MainScene -> m MeteorManager
updateMeteorManager scene = do
  greaterMeteorList <- checkPopNewMeteor scene newFrameCount' (meteorList meteorManager')
  let updatedMeteorList = map (updateMeteor scene) greaterMeteorList
  
  return meteorManager' 
    { managerFrameCount = newFrameCount'
    , meteorList = updatedMeteorList }
  where
    newFrameCount' = 1 + managerFrameCount meteorManager'
    meteorManager' = meteorManager scene



renderMeteor :: MonadIO m => SDL.Renderer -> ImageRsc -> Meteor -> m ()
renderMeteor r rsc meteor = do
  Rendering.renderPixelartCentral r (solid_rock_bud_16x16 rsc) dest $ SrcRect src cellSize
  where
    cellSize = Vec 16 16
    dest = toVecInt $ currPos meteor
    frameDuration = 15
    numFrame = 2
    srcX = getX cellSize * calcAnimFrameIndex numFrame frameDuration (animCount meteor)
    src = Vec srcX 0


renderMeteorManager :: MonadIO m => SDL.Renderer -> ImageRsc -> MeteorManager -> m ()
renderMeteorManager r rsc meteorManager' = do
  forM_ meteors render
  where 
    render = renderMeteor r rsc
    meteors = meteorList meteorManager'
  

