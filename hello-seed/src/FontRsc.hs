
module FontRsc where

import qualified SDL
import Control.Monad.IO.Class
import qualified SDLWrapper
import SDL.Font (Font)
import Control.Monad

data FontRsc = FontRsc
  { mplus64 :: Font
  }

loadFontRsc :: (MonadIO m) => (FontRsc -> m a) -> m()
loadFontRsc op =
  SDLWrapper.withFont "./assets/Mplus2-Regular.ttf" 24 $ \mplus64' ->
    
    void $ op (FontRsc
      { mplus64 = mplus64'
      })



