
module MainScene.EffectObjectBehavior where
import MainScene.EffectObject (EffectObject (OvalElem, OvalGen))
import World
import Control.Monad.Cont
import ImageRsc (ImageRenderer (ImageRenderer), ImageRsc (oval_16x16))
import Vec
import Rendering
import MainScene.MainScene
import MainScene.HarvestManager


refreshEffectObjects :: (MonadIO m) => World -> m [EffectObject]
refreshEffectObjects w = do
  forM_ effects $ renderEffect $ ImageRenderer (imageRsc w) (renderer w)
  --liftIO $ print $ length effects
  let updatedList =
        filter isAliveEffect $
        map (updateEffect ms)
        effects
  return $ checkBirthNewEffect ms ++ updatedList
  where
    ms = scene w
    effects = effectObjects ms




checkBirthNewEffect :: MainScene -> [EffectObject]
checkBirthNewEffect ms = (checkEffs . checkHarvs) []
  where
    currList = effectObjects ms

    checkHarvs = \temp -> foldr (\harv effs -> effs ++ checkBirthOvalGen harv) temp $ harvestList $ harvestManager ms
    checkEffs = \temp -> foldr (\e effs -> effs ++ generateEffect e) temp currList


-- 
checkBirthOvalGen :: Harvest -> [EffectObject]
checkBirthOvalGen harv = [OvalGen 0 $ toVecF $ installedPos harv | justCropped harv]


generateEffect :: EffectObject -> [EffectObject]

ovalGenDuration :: Int
ovalGenDuration = 2
ovalGenStepRad :: Int
ovalGenStepRad = 30
generateEffect (OvalGen count pos) =
  let
    num = count `div` ovalGenDuration
    r = 4 + num * 2
    rad = pi * fromIntegral num * fromIntegral ovalGenStepRad / (180 :: Float)
    delta = Vec (cos rad) (sin rad) ~* fromIntegral r
    v = Vec (cos rad) (sin rad) ~* 2
  in [OvalElem 0 (pos ~+ delta) v 
      | count `mod` ovalGenDuration == 0]


generateEffect _ = []







updateEffect :: MainScene -> EffectObject -> EffectObject

updateEffect _ (OvalGen count pos) = OvalGen (count+1) pos

updateEffect _ (OvalElem count pos vel) = OvalElem (count+1) newPos newVel
  where
    newPos = pos ~+ vel
    newVel = vel ~+ accel
    accel = Vec 0 0.1







isAliveEffect :: EffectObject -> Bool

isAliveEffect (OvalGen count _) = count < (360 `div` ovalGenStepRad) * ovalGenDuration

isAliveEffect (OvalElem count _ _) = count < (baseFps * 3) `div` 2








renderEffect :: MonadIO m => ImageRenderer -> EffectObject -> m()

renderEffect (ImageRenderer rsc r) (OvalElem count pos _) = do
  Rendering.renderTexture 2 r (oval_16x16 rsc)
    (toVecInt pos) $
    SrcRect (Vec 0 0) (Vec 16 16)

renderEffect _ _ = return ()


