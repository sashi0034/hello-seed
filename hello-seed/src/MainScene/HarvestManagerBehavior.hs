module MainScene.HarvestManagerBehavior where
import MainScene.HarvestManager (HarvestManager)
import Control.Monad.Cont
import World
import MainScene.MainScene (MainScene(harvestManager, screenSize))
import qualified Rendering
import Vec
import Rendering
import ImageRsc (ImageRsc(corn_24x24))


refreshHarvestManager :: (MonadIO m) => World -> m HarvestManager
refreshHarvestManager w = do
  Rendering.renderPixelartCentral
    (renderer w) (corn_24x24 $ imageRsc w) halfScSize $ SrcRect (Vec 0 0) (Vec 24 24)
  return $ harvestManager $ scene w

  where
    scSize = screenSize $ scene w
    halfScSize = toVecInt $ toVecF scSize ~* (0.5 :: Float)

