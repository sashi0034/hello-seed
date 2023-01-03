{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Scene.Scene where
import Vec
import Scene.Player
import Scene.Background
import Scene.MeteorManager (MeteorManager, initialMeteorManager)
import Scene.InfoUI
import Control.Monad.Cont
import Scene.HarvestManager (HarvestManager, initialHarvestManager)
import Scene.EffectObject (EffectObject)
import Types (FrameCount)
import qualified SDL
import ImageRsc
import FontRsc
import InputState
import Control.Lens
import qualified ConstParam
import SoundRsc (SoundRsc)
import qualified SDL.Mixer


data SceneState = Title | Playing deriving (Eq)

data PlayingRecord = PlayingRecord
  { _currScore :: Int
  , _highScore :: Int
  , _currLevel :: Int
  } deriving Show

makeLenses ''PlayingRecord


baseFps :: Int
baseFps = 60


data Environment = Environment
  { currentBaseFps :: Int
  , exiting :: Bool
  , window :: SDL.Window
  , renderer :: SDL.Renderer
  , imageRsc :: ImageRsc
  , fontRsc :: FontRsc
  , soundRsc :: SoundRsc
  , windowSize :: VecInt
  , input :: InputState
  }


data SceneMetaInfo = SceneMetaInfo
  { _sceneState :: SceneState
  , _sceneFrame :: FrameCount
  , _playingRecord :: PlayingRecord
  , _screenSize :: VecInt
  }


makeLenses ''SceneMetaInfo


data ActorUpdate =
    ActorUpdate (Scene -> Scene)
  | ActorUpdateIO (Scene -> IO Scene)


data ActorActive = ActorActive (Scene -> Bool)


data ActorRender =
    ActorRenderNone
  | ActorRenderIO (Scene -> IO ())


data ActorAct = ActorAct
  ActorUpdate
  ActorActive
  ActorRender


data Scene = Scene
  { _sceneEnv :: Environment
  , _sceneActorActList :: [ActorAct]
  , _sceneMetaInfo :: SceneMetaInfo
  , _scenePlayer :: Player
  , _sceneBackground :: Background
  , _sceneMeteorManager :: MeteorManager
  , _sceneInfoUI :: InfoUI
  , _sceneHarvestManager :: HarvestManager
  , _sceneEffectObjects :: [EffectObject]
  }


makeFields ''Scene
-- 上記により、  
--     class HasPlayer s a | s -> a where
--       player :: Lens' s a
--     instance HasX (Foo a) Int where
--       player = _scenePlayerLens
-- などが生成される


initialEnv :: SDL.Window -> SDL.Renderer -> ImageRsc -> FontRsc -> SoundRsc -> VecInt -> Environment
initialEnv window' renderer' imageRsc' fontRsc' soundRsc' windowSize' = Environment
  { exiting = False
  , currentBaseFps = baseFps
  , window = window'
  , renderer = renderer'
  , imageRsc = imageRsc'
  , fontRsc = fontRsc'
  , soundRsc = soundRsc'
  , windowSize = windowSize'
  , input = noInput
  }


initialSceneMetaInfo :: VecInt -> SceneMetaInfo
initialSceneMetaInfo screen = SceneMetaInfo
  { _sceneState = Title
  , _sceneFrame = 0
  , _playingRecord = PlayingRecord{ _currScore=0, _highScore=0, _currLevel=1 }
  , _screenSize = screen
  }


applyActUpdate :: ActorAct -> Scene -> IO Scene
applyActUpdate (ActorAct update _ _) s = case update of
    (ActorUpdate func) -> return $ func s
    (ActorUpdateIO func) -> func s


applyActActive :: ActorAct -> Scene -> Bool
applyActActive (ActorAct _ active _) s = case active of
    (ActorActive func) -> func s


applyActRender :: ActorAct -> Scene -> IO ()
applyActRender (ActorAct _ _ render) s = case render of
    ActorRenderNone -> return ()
    (ActorRenderIO func) -> func s



withScene :: MonadIO m => Environment -> VecInt -> (forall (m1 :: * -> *). (MonadIO m1) => Scene -> m1 ()) -> m()
withScene env' screenSize' ope =  (`runContT` return) $ do
  infoUI' <- ContT initialInfoUI

  let scene = Scene
        { _sceneEnv = env'
        , _sceneMetaInfo = initialSceneMetaInfo screenSize'
        , _sceneActorActList = []
        , _scenePlayer = initialPlayer screenSize'
        , _sceneBackground = initialBackground
        , _sceneMeteorManager = initialMeteorManager
        , _sceneHarvestManager = initialHarvestManager screenSize'
        , _sceneInfoUI = infoUI'
        , _sceneEffectObjects = []
        }

  ope scene


initPlaying :: Scene -> Scene
initPlaying s =
  let size = s ^. (metaInfo . screenSize)
      pr = s ^. (metaInfo . playingRecord)
      pr' = pr& currScore.~0 & currLevel.~ConstParam.initialLevel
  in s
  { _sceneMetaInfo = (s^.metaInfo) {_playingRecord = pr'}
  , _scenePlayer = initialPlayer size
  , _sceneBackground = initialBackground
  , _sceneMeteorManager = initialMeteorManager
  , _sceneHarvestManager = initialHarvestManager size
  }


isSceneState :: SceneState -> Scene -> Bool
isSceneState state s = state == s ^. (metaInfo . sceneState)


-- justCropped :: Scene -> Harvest -> Bool
-- justCropped s harv = whenCropped harv == (-1 + s ^. (metaInfo . sceneFrame))



isHitStopping :: (HasPlayer s Player) => s -> Bool
isHitStopping s =
  let ps = playerState $ s^.player
  in case ps of
      HitStopping _ -> True
      _ -> False


playSe :: (HasEnv s Environment, MonadIO m) => s -> (SoundRsc -> SDL.Mixer.Chunk) -> m()
playSe s se = 
  SDL.Mixer.play $ se (soundRsc $ s^.env)

