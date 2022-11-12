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
  , blue_bg :: SDL.Texture
  , solid_rock_bud_16x16 :: SDL.Texture
  , mail_16x16 :: SDL.Texture
  , octocat_16x16 :: SDL.Texture
  , corn_24x24 :: SDL.Texture
  , oval_16x16 :: SDL.Texture
  }


data ImageRenderer = ImageRenderer ImageRsc SDL.Renderer


loadImageRsc :: (MonadIO m) => SDL.Renderer -> (ImageRsc -> m a) -> m ()
loadImageRsc r op = 
  SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \blobwob_24x24' -> 
  SDLWrapper.withTexture r "./assets/tree_16x16.png" $ \tree_16x16' -> 
  SDLWrapper.withTexture r "./assets/solid_rock_bud_16x16.png" $ \solid_rock_bud_16x16' ->   
  SDLWrapper.withTexture r "./assets/mail_16x16.png" $ \mail_16x16' ->   
  SDLWrapper.withTexture r "./assets/octocat_16x16.png" $ \octocat_16x16' ->  
  SDLWrapper.withTexture r "./assets/corn_24x24.png" $ \corn_24x24' -> 
  SDLWrapper.withTexture r "./assets/oval_16x16.png" $ \oval_16x16' -> 

  -- <a href="https://www.freepik.com/free-vector/blue-curve-background_16282276.htm#query=simple%20background&position=7&from_view=keyword">Image by rawpixel.com</a> on Freepik
  --SDLWrapper.withTexture r "./assets/blue_bg.jpg" $ \blue_bg' -> 

  -- Image by <a href="https://www.freepik.com/free-vector/abstract-classic-blue-screensaver_6674908.htm#page=2&query=simple%20background&position=9&from_view=keyword">Freepik</a>
  SDLWrapper.withTexture r "./assets/blue_bg_dark.jpg" $ \blue_bg' ->   

    void $ op (ImageRsc 
      { blobwob_24x24 = blobwob_24x24'
      , tree_16x16 = tree_16x16'
      , blue_bg = blue_bg'
      , solid_rock_bud_16x16 = solid_rock_bud_16x16'
      , mail_16x16 = mail_16x16'
      , octocat_16x16 = octocat_16x16'
      , corn_24x24 = corn_24x24'
      , oval_16x16 = oval_16x16'
      })


