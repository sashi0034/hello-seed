
module MainScene.EffectObjectBehavior where
import MainScene.EffectObject (EffectObject (OvalElem, OvalGen))
import World
import Control.Monad.Cont
import ImageRsc (ImageRenderer (ImageRenderer), ImageRsc (oval_16x16))
import Vec
import Rendering
import MainScene.MainScene (MainScene (effectObjects))


refreshEffectObjects :: (MonadIO m) => World -> m [EffectObject]
refreshEffectObjects w = do
  forM_ effects $ renderEffect $ ImageRenderer (imageRsc w) (renderer w)
  return $
    filter isAliveEffect $ 
    map (updateEffect ms) $ 
    effects ++ checkBirthNewEffect ms
  where 
    ms = scene w
    effects = effectObjects ms




checkBirthNewEffect :: MainScene -> [EffectObject]
checkBirthNewEffect ms = foldr (\e effs -> effs ++ generateEffect e) [] currList
  where 
    currList = effectObjects ms
  --foldr (a -> b -> b) b (t a)


generateEffect :: EffectObject -> [EffectObject]
generateEffect _ = []







updateEffect :: MainScene -> EffectObject -> EffectObject

updateEffect _ (OvalGen count pos) = OvalGen (count+1) pos

updateEffect _ (OvalElem count pos vel) = OvalElem (count+1) newPos newVel
  where 
    newPos = pos
    newVel = vel







isAliveEffect :: EffectObject -> Bool

isAliveEffect (OvalGen count _) = count > baseFps * 2

isAliveEffect (OvalElem count _ _) = count > (baseFps * 3) `div` 2








renderEffect :: MonadIO m => ImageRenderer -> EffectObject -> m()

renderEffect (ImageRenderer rsc r) (OvalElem count pos _) = do
  Rendering.renderPixelartCentral r (oval_16x16 rsc) 
    (toVecInt pos) $ 
    SrcRect (Vec 0 0) (Vec 16 16)

renderEffect _ _ = return ()


