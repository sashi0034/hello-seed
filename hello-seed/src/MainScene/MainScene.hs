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

data MainScene = MainScene
  { sceneState :: SceneState
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
        , player = initialPlayer
        , background = initialBackground
        , meteorManager = initialMeteorManager
        , harvestManager = initialHarvestManager screenSize'
        , infoUI = infoUI'
        , screenSize = screenSize'
        }

  op scene


