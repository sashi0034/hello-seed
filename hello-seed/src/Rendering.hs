
module Rendering where
import qualified SDL
import Vec
import qualified SDLWrapper
import Control.Monad.IO.Class (MonadIO)


pixelartScale :: Int
pixelartScale = 3


data SrcRect = SrcRect VecInt VecInt


renderPixelart :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> VecInt -> SrcRect -> m()
renderPixelart renderer texture startDest (SrcRect startSrc size) =
  SDL.copy renderer texture (Just clipping) (Just dest)

  where
    srcX = getX startSrc
    srcY = getY startSrc
    srcW = getX size
    srcH = getY size

    clipping = fromIntegral <$> SDLWrapper.makeRect srcX srcY srcW srcH
    dest = fromIntegral <$> SDLWrapper.makeRect
      (getX startDest)
      (getY startDest)
      (pixelartScale * srcW)
      (pixelartScale * srcH)


renderPixelartCentral :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> VecInt -> SrcRect -> m()
renderPixelartCentral renderer texture startDest (SrcRect startSrc size) =
  renderPixelart renderer texture startDest' $ SrcRect startSrc size
  where
    halfSize = Vec (getX size `div` 2) (getY size `div` 2)
    startDest' = startDest ~- (halfSize ~* pixelartScale)




