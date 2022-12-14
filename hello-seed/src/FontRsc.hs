
module FontRsc where

import Control.Monad.IO.Class
import qualified SDLWrapper
import SDL.Font (Font)
import Control.Monad
import FontOutlined (FontOutlined, withFontOutlined)

data FontRsc = FontRsc
  { mplus24 :: Font
  , outlinedMplus24 :: FontOutlined
  , outlinedMplus96 :: FontOutlined
  }

withFontRsc :: (MonadIO m) => (FontRsc -> m a) -> m()
withFontRsc op =
  SDLWrapper.withFont "./assets/Mplus2-Regular.ttf" 24 $ \mplus24' ->
  withFontOutlined "./assets/Mplus2-Regular.ttf" 24 2 $ \outlinedMplus24' ->
  withFontOutlined "./assets/Mplus2-Regular.ttf" 96 4 $ \outlinedMplus96' -> 
    
    void $ op (FontRsc
      { mplus24 = mplus24'
      , outlinedMplus24 = outlinedMplus24'
      , outlinedMplus96 = outlinedMplus96'
      })
    
    -- liftIO $ print "freed font resource"



