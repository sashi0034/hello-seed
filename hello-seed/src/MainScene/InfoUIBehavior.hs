module MainScene.InfoUIBehavior where
import Control.Monad.Cont
import qualified SDL
import MainScene.InfoUI
import Rendering
import Vec
import Data.Text
import MainScene.Player (Player (pos))
import FontRsc


renderInfoUI :: (MonadIO m) => SDL.Renderer -> FontRsc -> Player -> InfoUI -> m ()
renderInfoUI r rsc player ui = do
  updateTextSolid r (mplus64 rsc) (SDL.V4 255 200 255 255) (pack pos'') (scoreText ui)
  renderPreRenderedText r (scoreText ui) (Vec 10 10)

  where 
    pos' = pos player
    pos'' = show pos'

