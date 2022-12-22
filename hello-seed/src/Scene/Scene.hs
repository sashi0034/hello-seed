{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Scene.Scene where
import Vec
import Scene.Player
import Scene.Background
import Scene.MeteorManager (MeteorManager, initialMeteorManager)
import Scene.InfoUI
import Control.Monad.Cont
import Scene.HarvestManager (HarvestManager, initialHarvestManager, Harvest (whenCropped))
import Scene.EffectObject (EffectObject)
import Types (FrameCount)
import qualified SDL
import ImageRsc
import FontRsc
import InputState
import Control.Lens


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
  , windowSize :: VecInt
  , input :: InputState
  }


data SceneMetaData = SceneMetaData
  { _sceneState :: SceneState
  , _sceneFrame :: FrameCount
  , _playingRecord :: PlayingRecord
  }


makeLenses ''SceneMetaData


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
  { _env :: Environment
  , _actorActList :: [] ActorAct
  , _sceneMeta :: SceneMetaData
  , _player :: Player
  , _background :: Background
  , _meteorManager :: MeteorManager
  , _infoUI :: InfoUI
  , _screenSize :: VecInt
  , _harvestManager :: HarvestManager
  , _effectObjects :: [] EffectObject
  }


makeLenses ''Scene


initialEnv :: SDL.Window -> SDL.Renderer -> ImageRsc -> FontRsc -> VecInt -> Environment
initialEnv window' renderer' imageRsc' fontRsc' windowSize' = Environment
  { exiting = False
  , currentBaseFps = baseFps
  , window = window'
  , renderer = renderer'
  , imageRsc = imageRsc'
  , fontRsc = fontRsc'
  , windowSize = windowSize'
  , input = noInput
  }


initialSceneMetaData = SceneMetaData
  { _sceneState = Title
  , _sceneFrame = 0
  , _playingRecord = PlayingRecord{ _currScore=0, _highScore=0, _currLevel=1 }
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
withScene env' screenSize' op =  (`runContT` return) $ do
  infoUI' <- ContT initialInfoUI

  let scene = Scene
        { _env = env'
        , _sceneMeta = initialSceneMetaData
        , _actorActList = []
        , _player = initialPlayer screenSize'
        , _background = initialBackground
        , _meteorManager = initialMeteorManager
        , _harvestManager = initialHarvestManager screenSize'
        , _infoUI = infoUI'
        , _screenSize = screenSize'
        , _effectObjects = []
        }

  op scene


initPlaying :: Scene -> Scene
initPlaying s =
  let size = s^.screenSize
      pr = s^.sceneMeta^.playingRecord
      pr' = pr& currScore.~0 & currLevel.~1
  in s
  { _sceneMeta = (s^.sceneMeta) {_playingRecord = pr'}
  , _player = initialPlayer size
  , _background = initialBackground
  , _meteorManager = initialMeteorManager
  , _harvestManager = initialHarvestManager size
  }


isSceneState :: SceneState -> Scene -> Bool
isSceneState state s = state == s^.sceneMeta^.sceneState


justCropped :: Scene -> Harvest -> Bool
justCropped s harv = whenCropped harv == (-1 + s^.sceneMeta^.sceneFrame)


isHitStopping :: Scene -> Bool
isHitStopping s =
  let ps = playerState $ s^.player
  in case ps of
      HitStopping _ -> True
      _ -> False
