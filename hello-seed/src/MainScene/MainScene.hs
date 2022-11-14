{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module MainScene.MainScene where
import Vec
import MainScene.Player
import MainScene.Background
import MainScene.MeteorManager (MeteorManager, initialMeteorManager)
import MainScene.InfoUI
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import Control.Monad.Cont
import MainScene.HarvestManager (HarvestManager, initialHarvestManager, Harvest (whenCropped))
import MainScene.EffectObject (EffectObject)
import Types (FrameCount)


data SceneState = Title | Playing

data PlayingRecord = PlayingRecord
  { currScore :: Int
  , highScore :: Int
  , currLevel :: Int
  }


data MainScene = MainScene
  { sceneState :: SceneState
  , sceneFrame :: FrameCount
  , playingRecord :: PlayingRecord
  , player :: Player
  , background :: Background
  , meteorManager :: MeteorManager
  , infoUI :: InfoUI
  , screenSize :: VecInt
  , harvestManager :: HarvestManager
  , effectObjects :: [] EffectObject
  }


withMainScene :: MonadIO m => VecInt -> (forall (m :: * -> *). (MonadIO m) => MainScene -> m ()) -> m()
withMainScene screenSize' op =  (`runContT` return) $ do
  infoUI' <- ContT initialInfoUI

  let scene = MainScene
        { sceneState = Title
        , sceneFrame = 0
        , playingRecord = PlayingRecord{ currScore=0, highScore=0, currLevel=1 }
        , player = initialPlayer screenSize'
        , background = initialBackground
        , meteorManager = initialMeteorManager
        , harvestManager = initialHarvestManager screenSize'
        , infoUI = infoUI'
        , screenSize = screenSize'
        , effectObjects = []
        }

  op scene


initPlaying :: MainScene -> MainScene
initPlaying s =
  let size = screenSize s
  in s
  { playingRecord = (playingRecord s){currScore=0, currLevel=1}
  , player = initialPlayer size
  , background = initialBackground
  , meteorManager = initialMeteorManager
  , harvestManager = initialHarvestManager size
  }


justCropped :: MainScene -> Harvest -> Bool
justCropped ms harv = whenCropped harv == (-1 + sceneFrame ms)


isHitStopping :: MainScene -> Bool
isHitStopping ms = 
  let ps = playerState $ player ms
  in case ps of
      HitStopping _ -> True
      _ -> False
