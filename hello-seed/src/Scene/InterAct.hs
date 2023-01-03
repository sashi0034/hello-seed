{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Scene.InterAct where
import Scene.Scene
import qualified Scene.HarvestManager as HarvestManager
import Scene.HarvestManager (HarvestManager(croppedStack, harvestList), Harvest (harvestPos), CroppedHarvest (CroppedHarvest), makeHarvestList, harvestCellSize)
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
import Control.Monad.IO.Class
import Control.Monad (when, unless)
import SoundRsc (SoundRsc(eat_enemy, eat_harvest, pacman_become, level_up))




interAct :: ActorAct
interAct = ActorAct
  (ActorUpdateIO updateCommunicator)
  (ActorActive $ const True)
  ActorRenderNone


updateCommunicator :: (MonadIO m) => Scene -> m Scene
updateCommunicator s =
      return s
  >>= onBecomePlayerPacman
  >>= onPacmanEatEnemy
  >>= onDestroyAllEnemies
  >>= onCroppedHarvest


-- トウモロコシが刈られたとき
onCroppedHarvest :: (MonadIO m) => Scene -> m Scene
onCroppedHarvest s =
  let hm = s ^. harvestManager
      cropped = HarvestManager.croppedStack hm
      len = length cropped

      effects = foldr (\c e -> e ++ birthOvalElem c) [] cropped
  in do
    when (len > 0) (playSe s eat_harvest)
    return $ s
      & harvestManager .~ hm { croppedStack = [] }
      & player %~ full %~ (`incFullness` len)
      & effectObjects %~ (++ effects)
    -- & metaInfo %~ (playingRecord . currScore) %~ (+ len)


-- パックマンになる
onBecomePlayerPacman :: (MonadIO m) => Scene -> m Scene
onBecomePlayerPacman s =
  case canBecomePacman $ s^.player of
    False -> return s
    True -> do
      playSe s pacman_become
      return $ s
        & player .~ ((s^.player) {playerState = Pacman 0} )
        & player %~ (full . currFull) .~ 0


-- パックマンで敵を食べた
onPacmanEatEnemy :: (MonadIO m) => Scene -> m Scene
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
            (SrcRect (Vec 0 0) $ metCellSize eaten)
            (metImage eaten)
      in do
        unless (null metEaten) (playSe s eat_enemy)
        return $ s
          & meteorManager .~ mm { metManagerElements = metAlive}
          & effectObjects %~ (++ concatMap eatenEffs metEaten)
          & metaInfo %~ (playingRecord . currScore) %~ (+ length metEaten)
    _ -> return s


-- 敵を全部倒した
onDestroyAllEnemies :: (MonadIO m) => Scene -> m Scene
onDestroyAllEnemies s =
  let mm = s^.meteorManager
  in case metManagerGenAble mm == 0 && null (metManagerElements mm)  of
      False -> return s
      True ->
        let nextLevel = s^.metaInfo^.playingRecord^.currLevel + 1
            scrapHarvs = concatMap (\h -> makeScrapEffect 0
              60
              2
              (toVecF $ harvestPos h)
              (SrcRect (Vec 0 0) harvestCellSize)
              (corn_24x24)) (harvestList $ s^.harvestManager)
        in do
          playSe s level_up
          return $ s
            & meteorManager .~ mm { metManagerGenAble = getMetGenAbleNext nextLevel }
            & metaInfo %~ (playingRecord . currLevel) .~ nextLevel
            & background .~ (s^.background) { bgNextInfo = Just $ BgNextInfo (getBgImageByLevel nextLevel) 0 }
            & harvestManager .~ (s^.harvestManager) { harvestList = makeHarvestList nextLevel (s ^. (metaInfo . screenSize)) }
            & effectObjects %~ (++ scrapHarvs)


