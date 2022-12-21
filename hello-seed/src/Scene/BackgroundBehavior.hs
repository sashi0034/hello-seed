
module Scene.BackgroundBehavior
( backgroundAct
) where

import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import qualified SDLWrapper
import Vec
import Scene.Scene
import Scene.Background
import SDL.Video




backgroundAct = ActorAct
  (ActorUpdate updateBackground)
  (ActorActive (const True))
  (ActorRenderIO renderBackground)


updateBackground :: Scene -> Scene
updateBackground s = s { background = bg'}
  where
    bg = background s
    bg' = bg { animCount = 1 + animCount bg }


renderBackground :: (MonadIO m) => Scene -> m ()
renderBackground s = do
  let r = renderer $ env s
      image = imageRsc $ env s
      bgTexture = blue_bg image

  bgTextureInfo <- queryTexture bgTexture

  let bgW = textureWidth bgTextureInfo
      bgH = textureHeight bgTextureInfo

  let applySrcPt value = fromIntegral $ floor $ maxAmp + value

  let srcX1 = applySrcPt currAmp
      srcY1 = applySrcPt currAmp
      srcX2 = applySrcPt $ -currAmp + fromIntegral bgW
      srcY2 = applySrcPt $ -currAmp + fromIntegral bgH

  let src = SDLWrapper.makeRect srcX1 srcY1 srcX2 srcY2

  SDL.copy r (blue_bg image) (Just src) (Just dest)

  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX size) (fromIntegral $ getY size)
    size = screenSize s

    currPhase = (fromIntegral (animCount $ background $ s) / 180) * pi :: Float
    maxAmp = 200 :: Float
    currAmp = maxAmp * sin currPhase



