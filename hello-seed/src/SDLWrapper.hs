module SDLWrapper where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text              (Text)

import SDL (($=))
import qualified GHC.IO as SDL
import Data.IORef

-- 参考: https://github.com/palf/haskell-sdl2-examples


data SurTex = SurTex (Maybe SDL.Surface) (Maybe SDL.Texture)


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit


withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit


withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  void op
  SDL.Font.quit


withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)


withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r


withTexture :: (MonadIO m) => SDL.Renderer -> SDL.FilePath -> (SDL.Texture -> m a) -> m ()
withTexture r filePath op = do
  t <- SDL.Image.loadTexture r filePath
  void $ op t
  SDL.destroyTexture t


withFont :: (MonadIO m) => FilePath -> SDL.Font.PointSize -> (SDL.Font.Font -> m a) -> m()
withFont path size op = do
  font <- SDL.Font.load path size
  void $ op font
  SDL.Font.free font


withNewSurTexRef ::  (MonadIO m) => (IORef SurTex -> m a) -> m()
withNewSurTexRef op = do
  surTexRef <- liftIO $ newIORef $ SurTex Nothing Nothing
  void $ op surTexRef
  endRef <- liftIO $ readIORef surTexRef
  freeSurTex endRef


freeSurTex :: (MonadIO m) => SurTex -> m()
freeSurTex (SurTex sur tex) = do
  mapM_ SDL.freeSurface sur
  mapM_ SDL.destroyTexture tex


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w


isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False


isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False


setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest


loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


makePoint :: a -> a -> SDL.Point SDL.V2 a
makePoint x y = SDL.P (SDL.V2 x y)


makeRect :: a -> a -> a -> a-> SDL.Rectangle a
makeRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h
