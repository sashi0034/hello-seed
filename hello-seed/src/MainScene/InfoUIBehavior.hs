module MainScene.InfoUIBehavior where
import Control.Monad.Cont
import qualified SDL
import MainScene.InfoUI
import Rendering
import Vec
import Data.Text
import MainScene.Player (Player (pos))
import FontRsc
import World
import MainScene.MainScene
import Data.IORef


refreshInfoUI :: (MonadIO m) => World -> m InfoUI
refreshInfoUI w = do
  renderInfoUI w $ infoUI $ scene w
  return $ infoUI $ scene w


renderInfoUI :: (MonadIO m) => World -> InfoUI -> m ()
renderInfoUI w ui = do

  let leftTop = Vec 64 32
  let space = 32
  renderText w (pack score) (scoreText ui) leftTop
  renderText w (pack high) (scoreText ui) (leftTop ~+ Vec 0 space)

  where
    ms = scene w
    pr = playingRecord ms
    score = "Curr Score :  " ++ show (currScore pr)
    high  = "High Score :  " ++ show (highScore pr)


renderText :: MonadIO m => World -> Text -> IORef RenderedText -> VecInt -> m ()
renderText w str tex start = do
  updateTextBlendedOutlined
    (renderer w) (outlinedMplus24 (fontRsc w))
    (SDL.V4 160 255 120 255) (SDL.V4 120 100 120 255)
    str tex
  renderPreRenderedText (renderer w) tex start