module MainScene.HarvestManagerBehavior where
import MainScene.HarvestManager (HarvestManager (harvestList), Harvest (installedPos))
import Control.Monad.Cont
import World
import MainScene.MainScene (MainScene(harvestManager, screenSize))
import Vec
import Rendering
import ImageRsc (ImageRsc(corn_24x24))


refreshHarvestManager :: (MonadIO m) => World -> m HarvestManager
refreshHarvestManager w = do
  forM_ (harvestList this) (renderHarvest w)
  return $ harvestManager $ scene w
  where 
    this = harvestManager $ scene w


renderHarvest :: (MonadIO m) => World -> Harvest -> m()
renderHarvest w h = do
  Rendering.renderPixelartCentral
    (renderer w) (corn_24x24 $ imageRsc w) pos $ SrcRect (Vec 0 0) (Vec 24 24)
  where 
    pos = installedPos h


