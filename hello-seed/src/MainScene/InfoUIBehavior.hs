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


refreshInfoUI :: (MonadIO m) => World -> m InfoUI
refreshInfoUI w = do
  renderInfoUI w $ infoUI $ scene w
  return $ infoUI $ scene w


renderInfoUI :: (MonadIO m) => World -> InfoUI -> m ()
renderInfoUI w ui = do
  updateTextSolid (renderer w) (mplus64 (fontRsc w)) (SDL.V4 255 200 255 255) (pack pos'') (scoreText ui)
  renderPreRenderedText (renderer w) (scoreText ui) (Vec 10 10)

  where 
    scene' = scene w
    pos' = pos $ player scene'
    pos'' = show pos'

