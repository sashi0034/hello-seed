module Scene.InfoUI
( InfoUI (..)
, TextTexCache(..)
, initialInfoUI
, UiFullness(UiFullCharging)
) where
import Data.IORef (IORef, newIORef)
import Rendering (RenderedText)
import SDLWrapper (withNewSurTexRef)
import Control.Monad.IO.Class
import Types (FrameCount)


data TextTexCache = TextTexCache (IORef RenderedText) (IORef String)


withNewTextTexCache :: (MonadIO m) => (TextTexCache -> m a) -> m()
withNewTextTexCache op = do
  str <- liftIO $ newIORef ""
  withNewSurTexRef $ \surTex -> 
    op $ TextTexCache surTex str


data UiFullness = UiFullCharging FrameCount


data InfoUI = InfoUI
  { uiFullness :: UiFullness
  , textScore :: TextTexCache
  , textHighScore :: TextTexCache
  , textLevel :: TextTexCache
  , textGameOver :: TextTexCache
  , textTitle :: TextTexCache
  , textTitlePas :: TextTexCache
  }


initialInfoUI :: MonadIO m => (InfoUI -> m ()) -> m ()
initialInfoUI op = 
  withNewTextTexCache $ \score -> 
  withNewTextTexCache $ \highScore -> 
  withNewTextTexCache $ \level -> 
  withNewTextTexCache $ \gameOver -> 
  withNewTextTexCache $ \title -> 
  withNewTextTexCache $ \titlePas -> 

    op $ InfoUI 
      { textScore=score
      , textHighScore=highScore
      , textLevel = level
      , textGameOver = gameOver
      , textTitle = title
      , textTitlePas = titlePas
      , uiFullness = UiFullCharging 0
      }


