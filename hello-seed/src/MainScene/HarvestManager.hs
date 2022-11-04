module MainScene.HarvestManager where

data HarvestManager = HarvestManager
  { harvestFrameCount :: Int }

initialHarvestManager :: HarvestManager
initialHarvestManager = HarvestManager
  { harvestFrameCount = 0 }

