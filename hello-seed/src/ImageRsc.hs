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
  , bg_a :: SDL.Texture
  , bg_b :: SDL.Texture
  , bg_c :: SDL.Texture
  , bg_d :: SDL.Texture
  , solid_rock_bud_16x16 :: SDL.Texture
  , mail_16x16 :: SDL.Texture
  , octocat_16x16 :: SDL.Texture
  , corn_24x24 :: SDL.Texture
  , oval_16x16 :: SDL.Texture
  , crying_laughing_16x16 :: SDL.Texture
  , logo_hungry :: SDL.Texture
  , logo_stuffed :: SDL.Texture
  }


data ImageRenderer = ImageRenderer ImageRsc SDL.Renderer


type ImageGetter = (ImageRsc -> SDL.Texture)


loadImageRsc :: (MonadIO m) => SDL.Renderer -> (ImageRsc -> m a) -> m ()
loadImageRsc r op = 
  SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \blobwob_24x24' -> 
  SDLWrapper.withTexture r "./assets/tree_16x16.png" $ \tree_16x16' -> 
  SDLWrapper.withTexture r "./assets/solid_rock_bud_16x16.png" $ \solid_rock_bud_16x16' ->   
  SDLWrapper.withTexture r "./assets/mail_16x16.png" $ \mail_16x16' ->   
  SDLWrapper.withTexture r "./assets/octocat_16x16.png" $ \octocat_16x16' ->  
  SDLWrapper.withTexture r "./assets/corn_24x24.png" $ \corn_24x24' -> 
  SDLWrapper.withTexture r "./assets/oval_16x16.png" $ \oval_16x16' -> 
  SDLWrapper.withTexture r "./assets/crying_laughing_16x16.png" $ \crying_laughing_16x16' -> 

  -- <a href="https://www.freepik.com/free-vector/blue-curve-background_16282276.htm#query=simple%20background&position=7&from_view=keyword">Image by rawpixel.com</a> on Freepik
  --SDLWrapper.withTexture r "./assets/bg_a.jpg" $ \bg_a' -> 

  -- Image by <a href="https://www.freepik.com/free-vector/abstract-classic-blue-screensaver_6674908.htm#page=2&query=simple%20background&position=9&from_view=keyword">Freepik</a>
  SDLWrapper.withTexture r "./assets/bg_a.jpg" $ \bg_a' ->   

  -- https://pngtree.com/freebackground/abstract-background-design-green-abstract-background-design-line-art-background-design_1235685.html
  SDLWrapper.withTexture r "./assets/bg_b.jpg" $ \bg_b' ->

  -- https://www.vecteezy.com/vector-art/2915060-abstract-colorful-background
  SDLWrapper.withTexture r "./assets/bg_c.jpg" $ \bg_c' ->

  -- https://www.vecteezy.com/vector-art/1527263-purple-geometric-shapes-background
  SDLWrapper.withTexture r "./assets/bg_d.jpg" $ \bg_d' ->

  SDLWrapper.withTexture r "./assets/logo_hungry.png" $ \logo_hungry' ->   
  SDLWrapper.withTexture r "./assets/logo_stuffed.png" $ \logo_stuffed' ->   

    void $ op (ImageRsc 
      { blobwob_24x24 = blobwob_24x24'
      , tree_16x16 = tree_16x16'
      , bg_a = bg_a'
      , bg_b = bg_b'
      , bg_c = bg_c'
      , bg_d = bg_d'
      , solid_rock_bud_16x16 = solid_rock_bud_16x16'
      , mail_16x16 = mail_16x16'
      , octocat_16x16 = octocat_16x16'
      , corn_24x24 = corn_24x24'
      , oval_16x16 = oval_16x16'
      , crying_laughing_16x16 = crying_laughing_16x16'
      , logo_hungry = logo_hungry'
      , logo_stuffed = logo_stuffed'
      })


