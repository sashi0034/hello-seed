module MainScene.InfoUIBehavior(refreshInfoUI) where
import Control.Monad.Cont
import qualified SDL
import MainScene.InfoUI
import Rendering
import Vec
import Data.Text
import FontRsc
import World
import MainScene.MainScene
import Data.IORef
import MainScene.Player
import qualified SDL.Font


refreshInfoUI :: (MonadIO m) => World -> m InfoUI
refreshInfoUI w = do
  renderInfoUI w $ infoUI $ scene w
  return $ infoUI $ scene w


renderInfoUI :: (MonadIO m) => World -> InfoUI -> m ()
renderInfoUI w ui = do
  let ms= scene w

  let leftTop = Vec 64 32
  let rightTop = Vec (getX (screenSize ms) - 64) 32
  let space = 32
  renderText w score (textScore ui) leftTop LeftTop DefaultStyle
  renderText w high (textHighScore ui) (leftTop ~+ Vec 0 space) LeftTop DefaultStyle

  renderText w lv (textLevel ui) rightTop RightTop DefaultStyle

  when
    -- Game Over
    (sceneState ms == Playing && countAfterDiedPlayer (playerState $ player ms) > 0)
    $ renderText w "Game Over" (textGameOver ui) 
        (screenSize ms `divVec` 2) 
        MiddleCenter $ Header $ SDL.V4 255 120 80 255

  when
    -- Title
    (sceneState ms == Title)
    $ do 
      renderText w "Full Up Blobwov" (textTitle ui) 
        (screenSize ms `divVec` 2) 
        MiddleCenter $ Header $ SDL.V4 200 255 80 255
      renderText w "Press Left Click To Start" (textTitlePas ui) 
        (screenSize ms `divVec` 2 ~+ Vec 0 128) 
        MiddleCenter DefaultStyle


  where
    ms = scene w
    pr = playingRecord ms
    score = "Curr Score :  " ++ show (currScore pr)
    high  = "High Score :  " ++ show (highScore pr)
    lv = "Level :  " ++ show (currLevel pr)


data TextAlign = LeftTop | RightTop | MiddleCenter


data Style = DefaultStyle | Header SDL.Font.Color


renderText :: MonadIO m => World -> String -> TextTexCache -> VecInt -> TextAlign -> Style -> m ()
renderText w str (TextTexCache tex buff) start align style = do
  beforeBuff <- liftIO $ readIORef buff
  
  when 
    -- キャッシュを取ってテキストに変化があるときだけ描画する
    (beforeBuff /= str) 
    $ do 
      liftIO $ writeIORef buff str
      case style of
        DefaultStyle -> updateTextBlendedOutlined
          (renderer w) (outlinedMplus24 (fontRsc w))
          (SDL.V4 160 255 120 255) (SDL.V4 120 100 120 255)
          (pack str) tex
        Header color -> updateTextBlendedOutlined
          (renderer w) (outlinedMplus96 (fontRsc w))
          color (SDL.V4 80 80 80 255)
          (pack str) tex

  start' <- case align of
        LeftTop -> return start
        RightTop -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- Vec (getX texSize) 0
        MiddleCenter -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- (texSize `divVec` 2)

  renderPreRenderedText (renderer w) tex start'