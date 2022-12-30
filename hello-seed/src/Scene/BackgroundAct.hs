
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
import AnimUtil
import Ease




bgChangeAnimDuration :: Int
bgChangeAnimDuration = 60 * 3


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
        if frame < bgChangeAnimDuration
          then
            -- 切り替わってる途中
            bg' { bgNextInfo = Just $ BgNextInfo image (frame + 1)}
          else
            -- 切り替え完了
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

  let currPhase = (fromIntegral (bgAnimCount $ s ^. background) / 180) * pi :: Float
      maxAmp = 120 :: Float
      currAmp = maxAmp * (1 + sin currPhase)

  let srcX1 = floor currAmp
      srcY1 = floor currAmp
      srcX2 = floor $ -currAmp * 2 + fromIntegral bgW
      srcY2 = floor $ -currAmp * 2 + fromIntegral bgH

  let srcClip = SDLWrapper.makeRect srcX1 srcY1 srcX2 srcY2

  -- 描画
  SDL.copy r bgTexture (Just srcClip) (Just dest)
  
  case bgNextInfo bg of
    Nothing -> return ()
    (Just (BgNextInfo nextImage nextAnimCount)) -> do
      -- 次のBGに切り替わる演出
      let centerX = getX scSize `div` 2
          centerY = getY scSize `div` 2
          stretchEase max = floor $ valueWithEaseBegin 
              (backOut (Overshoot 5)) 
              (RangeF 0 $ fromIntegral max) 
              bgChangeAnimDuration 
              nextAnimCount 
          stretchW = stretchEase $ getX scSize
          stretchH = stretchEase $ getY scSize
      
      SDL.copy r (nextImage image) Nothing 
        (Just $ SDLWrapper.makeRect 
          (fromIntegral $ centerX - (stretchW `div` 2)) 
          (fromIntegral $ centerY - (stretchH `div` 2)) 
          (fromIntegral stretchW) 
          (fromIntegral stretchH))
      return ()

  where
    dest = SDLWrapper.makeRect 0 0 (fromIntegral $ getX scSize) (fromIntegral $ getY scSize)
    scSize = s^. (metaInfo . screenSize)




