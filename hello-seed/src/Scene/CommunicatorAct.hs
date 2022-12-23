
module Scene.CommunicatorAct where
import Scene.Scene
import qualified Scene.HarvestManager as HarvestManager
import Scene.HarvestManager (HarvestManager(croppedStack), Harvest, CroppedHarvest (CroppedHarvest))
import Control.Lens
import Scene.EffectObject
import Vec
import Rendering
import Scene.EffectObjectAct
import Scene.Player




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

  in s
    & harvestManager .~ hm { croppedStack = [] }
    & player %~ full %~ (`incFullness` len)
    & effectObjects %~ (++ effects)
    & metaInfo %~ (playingRecord . currScore) %~ (+ len)



