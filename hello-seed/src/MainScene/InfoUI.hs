module MainScene.InfoUI where
import Data.IORef (IORef)
import Rendering (RenderedText)
import SDLWrapper (withNewSurTexRef)
import Control.Monad.IO.Class


data InfoUI = InfoUI
  { scoreText :: IORef RenderedText
  }


initialInfoUI :: MonadIO m => (InfoUI -> m ()) -> m ()
initialInfoUI op = 
  withNewSurTexRef $ \ref -> do
    op $ InfoUI ref


