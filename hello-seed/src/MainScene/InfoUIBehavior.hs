module MainScene.InfoUIBehavior where
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
  renderText w (pack score) (textScore ui) leftTop LeftTop
  renderText w (pack high) (textHighScore ui) (leftTop ~+ Vec 0 space) LeftTop

  renderText w (pack lv) (textLevel ui) rightTop RightTop

  when 
    (countAfterDiedPlayer (playerState $ player ms) > 0)
    $ renderText w (pack gameOver) (textGameOver ui) (screenSize ms `divVec` 2) MiddleCenter


  where
    ms = scene w
    pr = playingRecord ms
    score = "Curr Score :  " ++ show (currScore pr)
    high  = "High Score :  " ++ show (highScore pr)
    lv = "Level :  " ++ show (currLevel pr)
    gameOver = "Game Over"


data TextAlign = LeftTop | RightTop | MiddleCenter


renderText :: MonadIO m => World -> Text -> IORef RenderedText -> VecInt -> TextAlign -> m ()
renderText w str tex start align = do
  updateTextBlendedOutlined
    (renderer w) (outlinedMplus24 (fontRsc w))
    (SDL.V4 160 255 120 255) (SDL.V4 120 100 120 255)
    str tex

  start' <- case align of
        LeftTop -> return start
        RightTop -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- Vec (getX texSize) 0
        MiddleCenter -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- (texSize `divVec` 2)

  renderPreRenderedText (renderer w) tex start'