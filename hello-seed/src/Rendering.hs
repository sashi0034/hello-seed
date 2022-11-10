
module Rendering where
import qualified SDL
import Vec
import qualified SDLWrapper
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, readIORef, writeIORef)
import SDL.Font
import Data.Text
import Control.Monad
import Data.Maybe (fromMaybe)
import FontOutlined (FontOutlined (FontOutlined))
import Linear
import Linear.Affine (Point(P))


pixelartScale :: Int
pixelartScale = 3


data SrcRect = SrcRect VecInt VecInt


renderTexture :: (MonadIO m) => Int -> SDL.Renderer -> SDL.Texture -> VecInt -> SrcRect -> m()
renderTexture dotScale renderer texture startDest (SrcRect startSrc size) =
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
      (dotScale * srcW)
      (dotScale * srcH)


getTextureSize :: (MonadIO m) => SDL.Texture -> m VecInt
getTextureSize tex = do
  info <- SDL.queryTexture tex
  let w = SDL.textureWidth info
  let h = SDL.textureHeight info
  return $ Vec (fromIntegral w) (fromIntegral h)


renderPixelart :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> VecInt -> SrcRect -> m()
renderPixelart = renderTexture pixelartScale


renderPixelartCentral :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> VecInt -> SrcRect -> m()
renderPixelartCentral renderer texture startDest (SrcRect startSrc size) =
  renderPixelart renderer texture startDest' $ SrcRect startSrc size
  where
    halfSize = Vec (getX size `div` 2) (getY size `div` 2)
    startDest' = startDest ~- (halfSize ~* pixelartScale)


type RenderedText = SDLWrapper.SurTex


updateTextSolid :: (MonadIO m) => SDL.Renderer -> Font -> Color -> Text -> IORef RenderedText -> m()
updateTextSolid renderer font color text renderedRef = do
  renderedText <- liftIO $ readIORef renderedRef
  SDLWrapper.freeSurTex renderedText

  newSurface <- SDL.Font.solid font color text
  newTexture <- SDL.createTextureFromSurface renderer newSurface

  liftIO $ writeIORef renderedRef $ SDLWrapper.SurTex (Just newSurface) (Just newTexture)


updateTextBlendedOutlined :: (MonadIO m) => SDL.Renderer -> FontOutlined -> Color -> Color -> Text -> IORef RenderedText -> m()
updateTextBlendedOutlined renderer (FontOutlined fore edge outlineW) fg bg text renderedRef = do
  renderedText <- liftIO $ readIORef renderedRef
  SDLWrapper.freeSurTex renderedText

  -- フチの中身
  tempSurface <- SDL.Font.blended fore fg text

  -- フォントのフチに中身を乗せる
  newSurface <- SDL.Font.blended edge bg text
  let lineW = fromIntegral outlineW
  void $ SDL.surfaceBlit tempSurface Nothing newSurface (Just $ P $ V2 lineW lineW)

  SDL.freeSurface tempSurface
  
  newTexture <- SDL.createTextureFromSurface renderer newSurface

  liftIO $ writeIORef renderedRef $ SDLWrapper.SurTex (Just newSurface) (Just newTexture)


getTexFromRenderedTextRef :: (MonadIO m) => IORef RenderedText -> m (Maybe SDL.Texture)
getTexFromRenderedTextRef renderedRef = do
  renderedText <- liftIO $ readIORef renderedRef
  return $ popTexture renderedText
  where
    popTexture (SDLWrapper.SurTex _ t) = t


renderPreRenderedText :: (MonadIO m) => SDL.Renderer -> IORef RenderedText -> VecInt -> m()
renderPreRenderedText r textRef pos = do
  (SDLWrapper.SurTex _ tex) <- liftIO $ readIORef textRef
  texSize <- mapM getTextureSize tex
  let texSize' = fromMaybe vecZero texSize

  let render t = renderTexture 1 r t pos (SrcRect vecZero texSize')

  mapM_ render tex


