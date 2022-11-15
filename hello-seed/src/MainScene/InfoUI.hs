module MainScene.InfoUI where
import Data.IORef (IORef)
import Rendering (RenderedText)
import SDLWrapper (withNewSurTexRef)
import Control.Monad.IO.Class


data InfoUI = InfoUI
  { textScore :: IORef RenderedText
  , textHighScore :: IORef RenderedText
  , textLevel :: IORef RenderedText
  , textGameOver :: IORef RenderedText
  }


initialInfoUI :: MonadIO m => (InfoUI -> m ()) -> m ()
initialInfoUI op = 
  withNewSurTexRef $ \score ->
  withNewSurTexRef $ \highScore ->
  withNewSurTexRef $ \level ->
  withNewSurTexRef $ \gameOver ->

    op $ InfoUI 
      { textScore=score
      , textHighScore=highScore
      , textLevel = level
      , textGameOver = gameOver
      }


