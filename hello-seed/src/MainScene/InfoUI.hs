module MainScene.InfoUI where
import Data.IORef (IORef)
import Rendering (RenderedText)
import SDLWrapper (withNewSurTexRef)
import Control.Monad.IO.Class


data InfoUI = InfoUI
  { scoreText :: IORef RenderedText
  , highScoreText :: IORef RenderedText
  , levelText :: IORef RenderedText
  }


initialInfoUI :: MonadIO m => (InfoUI -> m ()) -> m ()
initialInfoUI op = 
  withNewSurTexRef $ \score ->
  withNewSurTexRef $ \highScore ->
  withNewSurTexRef $ \level ->

    op $ InfoUI 
      { scoreText=score
      , highScoreText=highScore
      , levelText = level
      }


