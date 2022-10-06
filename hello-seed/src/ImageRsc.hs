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
  SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \blobwob_24x24' -> 
  SDLWrapper.withTexture r "./assets/tree_16x16.png" $ \tree_16x16' -> 
    void $ op (ImageRsc 
      { blobwob_24x24 = blobwob_24x24'
      , tree_16x16 = tree_16x16'
      })


