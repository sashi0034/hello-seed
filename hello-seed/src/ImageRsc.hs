{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ImageRsc where


import qualified SDL
import Control.Monad.IO.Class
import qualified SDLWrapper
import Control.Monad          (void)

data ImageRsc = ImageRsc
  { blobwob_24x24 :: SDL.Texture
  , tree_16x16 :: SDL.Texture
  }

loadImageRsc :: (MonadIO m) => SDL.Renderer -> (ImageRsc -> m a) -> m ()
loadImageRsc r op = 
  SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \t0 -> 
  SDLWrapper.withTexture r "./assets/tree_16x16.png" $ \t1 -> 
    void $ op $ ImageRsc t0 t1


