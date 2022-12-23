
module Scene.CommunicatorAct where
import Scene.Scene
import qualified Scene.HarvestManager as HarvestManager
import Scene.HarvestManager (HarvestManager(croppedStack), Harvest, CroppedHarvest (CroppedHarvest))
import Control.Lens
import Scene.EffectObject
import Vec
import Rendering



communicatorAct :: ActorAct
communicatorAct = ActorAct
  (ActorUpdate updateCommunicator)
  (ActorActive $ const True)
  ActorRenderNone


updateCommunicator :: Scene -> Scene
updateCommunicator s = 
  onCroppedHarvest s


-- トウモロコシが刈られたとき
onCroppedHarvest :: Scene -> Scene
onCroppedHarvest s =
  let hm = s ^. harvestManager
      cropped = HarvestManager.croppedStack hm
      len = length cropped

      effects = foldr (\c e -> e ++ birthOvalElem c) [] cropped
      meta' = over (playingRecord . currScore) (+ len) (s ^. metaInfo)

  in s 
    & harvestManager .~ hm { croppedStack = [] }
    & effectObjects %~ (++ effects)
    & metaInfo .~ meta'


-- 刈り取られたときに小判を生成
birthOvalElem :: CroppedHarvest -> [EffectObject]
birthOvalElem (CroppedHarvest pos) = 
    [(\(x, y) -> OvalElem
        0
        (toVecF $ pos ~+ (Vec x y ~* pixelartScale ~* 4))
        (Vec 0 $ - 4)
        $ 2 * abs (2 + y))
      (x0, y0) | x0 <- [-2 .. 2], y0 <- [-2 .. 2]]


