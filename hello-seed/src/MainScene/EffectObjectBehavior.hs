{-# OPTIONS_GHC -Wno-type-defaults #-}

module MainScene.EffectObjectBehavior where
import MainScene.EffectObject (EffectObject (OvalElem, BlobElem))
import World
import Control.Monad.Cont
import ImageRsc (ImageRenderer (ImageRenderer), ImageRsc (oval_16x16, crying_laughing_16x16))
import Vec
import Rendering
import MainScene.MainScene
import MainScene.HarvestManager
import MainScene.Player as Player


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
checkBirthNewEffect ms = (checkPlayer . checkEffs . checkHarvs) []
  where
    currList = effectObjects ms

    checkHarvs = \temp -> foldr (\harv effs -> effs ++ checkBirthOvalElem ms harv) temp $ harvestList $ harvestManager ms
    checkPlayer = \temp -> temp ++ checkBirthBlobElem ms
    checkEffs = \temp -> foldr (\e effs -> effs ++ generateEffect e) temp currList


checkBirthOvalElem :: MainScene -> Harvest -> [EffectObject]
checkBirthOvalElem ms harv = if justCropped ms harv
  then
    [(\(x, y) -> OvalElem
        0 
        (toVecF $ pos ~+ (Vec x y ~* pixelartScale ~* 4)) 
        (Vec 0 $ - 4) 
        $ 2 * abs (2 + y))
      (x0, y0) | x0 <- [-2 .. 2], y0 <- [-2 .. 2]]
  else []
  where
    pos = installedPos harv

checkBirthBlobElem :: MainScene -> [EffectObject]
checkBirthBlobElem ms = case sceneState ms of
  Playing -> let p = player ms in case playerState p of
    (Player.Dead count) | count `mod` interval==0 -> 
      [BlobElem 0 start (v i ~* speed) | i <- [-6 .. 6]]
      where 
        interval = 10
        start = Player.pos p
        v = \i -> let rad = pi*i*30/180 in Vec (cos rad) (sin rad)
        speed = 5
    _ -> []
  _ -> []
  


generateEffect :: EffectObject -> [EffectObject]

generateEffect _ = []







updateEffect :: MainScene -> EffectObject -> EffectObject

updateEffect _ (OvalElem count pos vel delay) = if delay <= 0
  then OvalElem (count+1) newPos newVel 0
  else OvalElem count pos vel $ delay-1
  where
    newPos = pos ~+ vel
    newVel = vel ~+ accel
    accel = Vec 0 0.1

updateEffect _ (BlobElem count pos vec) = 
  BlobElem (count+1) (pos~+vec) vec






isAliveEffect :: EffectObject -> Bool

isAliveEffect (OvalElem count _ _ _) = count < (baseFps * 2) `div` 2

isAliveEffect (BlobElem count _ _) = count < (baseFps * 1) `div` 2






renderEffect :: MonadIO m => ImageRenderer -> EffectObject -> m()

renderEffect (ImageRenderer rsc r) (OvalElem _ pos _ _) = do
  Rendering.renderTexture 1 r (oval_16x16 rsc)
    (vecUnit ~* (-8*1) ~+ toVecInt pos) $
    SrcRect (Vec 0 0) (Vec 16 16)

renderEffect (ImageRenderer rsc r) (BlobElem _ pos _) = do
  Rendering.renderPixelartCentral r (crying_laughing_16x16 rsc)
    (toVecInt pos) $
    SrcRect (Vec 0 0) (Vec 16 16)



