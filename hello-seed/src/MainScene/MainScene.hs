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
import MainScene.HarvestManager (HarvestManager, initialHarvestManager)


data SceneState = Title | Playing

data PlayingRecord = PlayingRecord
  { currScore :: Int
  , highScore :: Int
  , currLevel :: Int
  }


data MainScene = MainScene
  { sceneState :: SceneState
  , playingRecord :: PlayingRecord
  , player :: Player
  , background :: Background
  , meteorManager :: MeteorManager
  , infoUI :: InfoUI
  , screenSize :: VecInt
  , harvestManager :: HarvestManager
  }


withMainScene :: MonadIO m => VecInt -> (forall (m :: * -> *). (MonadIO m) => MainScene -> m ()) -> m() 
withMainScene screenSize' op =  (`runContT` return) $ do
  infoUI' <- ContT initialInfoUI

  let scene = MainScene
        { sceneState = Title
        , playingRecord = PlayingRecord{ currScore=0, highScore=0, currLevel=1 }
        , player = initialPlayer screenSize'
        , background = initialBackground
        , meteorManager = initialMeteorManager
        , harvestManager = initialHarvestManager screenSize'
        , infoUI = infoUI'
        , screenSize = screenSize'
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
  