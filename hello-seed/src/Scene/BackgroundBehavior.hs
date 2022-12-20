
module Scene.BackgroundBehavior
( refreshBackground
) where

import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import qualified SDLWrapper
import Vec
import Scene.Scene
import Scene.Background
import SDL.Video



refreshBackground :: MonadIO m => Scene -> m Background
refreshBackground s = do
  renderBackground (renderer $ env s) (imageRsc $ env s) s
  updateBackground s


updateBackground :: MonadIO m => Scene -> m Background
updateBackground scene = do
  return background' { animCount = animCount background' + 1 }
  where
    background' = background scene



renderBackground :: (MonadIO m) => SDL.Renderer -> ImageRsc -> Scene -> m ()
renderBackground r imageRsc s = do
  --liftIO $ print $ animCount $ background $ scene world

  let bgTexture = blue_bg imageRsc
  bgTextureInfo <- queryTexture bgTexture
  
  let bgW = textureWidth bgTextureInfo
  let bgH = textureHeight bgTextureInfo

  let applySrcPt value = fromIntegral $ floor $ maxAmp + value
  let srcX1 = applySrcPt currAmp
  let srcY1 = applySrcPt currAmp
  let srcX2 = applySrcPt $ -currAmp + fromIntegral bgW
  let srcY2 = applySrcPt $ -currAmp + fromIntegral bgH

  let src = SDLWrapper.makeRect srcX1 srcY1 srcX2 srcY2

  SDL.copy r (blue_bg imageRsc) (Just src) (Just dest)

  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX size) (fromIntegral $ getY size)
    size = screenSize s

    currPhase = (fromIntegral (animCount $ background $ s) / 180) * pi :: Float
    maxAmp = 200 :: Float
    currAmp = maxAmp * sin currPhase



