module FontOutlined where
import SDL.Font
import qualified SDLWrapper
import Control.Monad.Cont


type FontForeground = Font
type FontEdge = Font
type OutlineThickness = Int
data FontOutlined = FontOutlined FontForeground FontEdge OutlineThickness


withFontOutlined :: (MonadIO m) => FilePath -> PointSize -> Int -> (FontOutlined -> m ()) -> m ()
withFontOutlined path fontSize outline op = do
  SDLWrapper.withFont path fontSize $ \fg ->
    SDLWrapper.withFont path fontSize $ \edge -> do
      setOutline edge outline
      op $ FontOutlined fg edge outline
  