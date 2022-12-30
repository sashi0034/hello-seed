{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Scene.InterAct where
import Scene.Scene
import qualified Scene.HarvestManager as HarvestManager
import Scene.HarvestManager (HarvestManager(croppedStack), Harvest, CroppedHarvest (CroppedHarvest))
import Control.Lens
import Scene.EffectObject
import Vec
import Rendering
import Scene.EffectObjectAct
import Scene.Player
import qualified Scene.Player as Player
import Scene.MeteorManager
import CollisionUtil (hitRectRect)
import Data.List
import qualified Scene.MeteorManager as MeteorManager
import Scene.Background (Background(bgNextInfo), BgNextInfo (BgNextInfo), getBgImageByLevel)
import ImageRsc




interAct :: ActorAct
interAct = ActorAct
  (ActorUpdate updateCommunicator)
  (ActorActive $ const True)
  ActorRenderNone


updateCommunicator :: Scene -> Scene
updateCommunicator s =
    onBecomePlayerPacman
  $ onCroppedHarvest
  $ onPacmanEatEnemy
  $ onDestroyAllEnemies s


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
    -- & metaInfo %~ (playingRecord . currScore) %~ (+ len)


-- パックマンになる
onBecomePlayerPacman :: Scene -> Scene
onBecomePlayerPacman s =
  case canBecomePacman $ s^.player of
    False -> s
    True -> s
      & player .~ ((s^.player) {playerState = Pacman 0} )
      & player %~ (full . currFull) .~ 0


-- パックマンで敵を食べた
onPacmanEatEnemy :: Scene -> Scene
onPacmanEatEnemy s = let p = s^.player
  in case playerState p of
    Pacman frame ->
      let colPac = colRectPacman p
          mm = s^.meteorManager
          (metEaten, metAlive) = partition
            (hitRectRect colPac . colRectMeteor) (metManagerElements mm)
          eatenEffs eaten = makeScrapEffect1
            60
            2
            (MeteorManager.metPos eaten)
            (SrcRect (Vec 0 0) meteorCellSize)
            (metImage eaten)
      in s
        & meteorManager .~ mm { metManagerElements = metAlive}
        & effectObjects %~ (++ concatMap eatenEffs metEaten)
        & metaInfo %~ (playingRecord . currScore) %~ (+ length metEaten)
    _ -> s


-- 敵を全部倒した
onDestroyAllEnemies :: Scene -> Scene
onDestroyAllEnemies s =
  let mm = s^.meteorManager
  in case metManagerGenAble mm == 0 && null (metManagerElements mm)  of
      False -> s
      True ->
        let nextLevel = s^.metaInfo^.playingRecord^.currLevel + 1
        in s
          & meteorManager .~ mm { metManagerGenAble = getMetGenAbleNext }
          & metaInfo %~ (playingRecord . currLevel) .~ nextLevel
          & background .~ (s^.background) { bgNextInfo = Just $ BgNextInfo (getBgImageByLevel nextLevel) 0 }


