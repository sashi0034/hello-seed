
module Scene.BackgroundAct
( updateBackground
, renderBackground
) where

import Control.Monad.IO.Class
import qualified SDL
import ImageRsc
import qualified SDLWrapper
import Vec
import Scene.Scene
import Scene.Background
import SDL.Video
import Control.Lens




updateBackground :: Scene -> Background
updateBackground s =
  let
    bg = s^.background

    bg' = bg
            { bgAnimCount = 1 + bgAnimCount bg
            , bgNextInfo = Nothing
            }
  in
    case bgNextInfo bg of
      Nothing -> bg'
      (Just (BgNextInfo image frame)) ->
        bg' { bgCurrImage =  image
            , bgNextInfo = Nothing}


renderBackground :: (MonadIO m) => Scene -> m ()
renderBackground s = do
  let bg = s^.background
      r = renderer $ s^.env
      image = imageRsc $ s^.env
      bgTexture = bgCurrImage bg image

  bgTextureInfo <- queryTexture bgTexture

  let bgW = textureWidth bgTextureInfo
      bgH = textureHeight bgTextureInfo

  let applySrcPt value = floor $ maxAmp + value

  let srcX1 = applySrcPt currAmp
      srcY1 = applySrcPt currAmp
      srcX2 = applySrcPt $ -currAmp + fromIntegral bgW
      srcY2 = applySrcPt $ -currAmp + fromIntegral bgH

  let src = SDLWrapper.makeRect srcX1 srcY1 srcX2 srcY2

  SDL.copy r (bg_a image) (Just src) (Just dest)

  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX size) (fromIntegral $ getY size)
    size = s^. (metaInfo . screenSize)

    currPhase = (fromIntegral (bgAnimCount $ s ^. background) / 180) * pi :: Float
    maxAmp = 200 :: Float
    currAmp = maxAmp * sin currPhase



