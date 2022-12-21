

module Scene.SceneAct
  ( setupScene
  , refreshScene
  ) where

import Control.Monad.IO.Class
import Scene.Scene
import Scene.PlayerAct
import Scene.BackgroundAct
import Scene.MeteorManagerAct
import Scene.InfoUIAct
import Scene.HarvestManagerAct
import InputState
import qualified SDL
import qualified Scene.HarvestManager as HarvestManager
import Scene.EffectObjectAct
import Scene.Player
import Data.Foldable (foldrM)
import Control.Monad


setupScene :: Scene -> Scene
setupScene s =
  let acts =
        [ backgroundAct
        , harvestManagerAct
        , effectObjectsAct
        , playerAct
        , meteorManagerAct
        , infoUIAct
        , sceneMetaAct
        ]
  in s{ actorActList = acts }


refreshScene :: (MonadIO m) => Scene -> m Scene
refreshScene s = do
  let nextFrame = 1 + sceneFrame s
      s1 = s { sceneFrame = nextFrame }
      s2 = checkShiftScene s1 $ sceneState s1
      acts = filter (`applyActActive` s2) $ actorActList s2

  s' <- liftIO $ foldrM applyActUpdate s2 acts

  liftIO $ forM_ acts (`applyActRender` s')

  return s'


checkShiftScene :: Scene -> SceneState -> Scene
checkShiftScene s Title =
  let butt = mouseButton $ mouse $ input $ env s
      isClicked = butt SDL.ButtonLeft
  in if isClicked
    then initPlaying $ s {sceneState = Playing}
    else s


checkShiftScene s Playing =
  let isContinued = countAfterDiedPlayer (playerState $ player s) < baseFps * 3
  in if isContinued
    then s
    else s {sceneState = Title}


calcScore :: Scene -> Int
calcScore s =
  let hl = HarvestManager.harvestList $ harvestManager s
      curr = currScore $ playingRecord s
  in foldr (\h n -> if justCropped s h then n+1 else n) curr hl


updatePlayingRecord :: Scene -> PlayingRecord
updatePlayingRecord s =
  let pr = playingRecord s
      newScore = calcScore s
  in pr
      { currScore = calcScore s
      , highScore = max newScore $ highScore pr}


sceneMetaAct :: ActorAct
sceneMetaAct = ActorAct
  (ActorUpdate $ \s -> s {playingRecord = updatePlayingRecord s} )
  (ActorActive $ activeInSceneWhen Playing)
  ActorRenderNone
