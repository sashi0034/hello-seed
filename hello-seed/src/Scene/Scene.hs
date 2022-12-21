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

data Scene = Scene
  { env :: Environment
  , actorActList :: [] ActorAct
  , sceneMeta :: SceneMetaData
  , player :: Player
  , background :: Background
  , meteorManager :: MeteorManager
  , infoUI :: InfoUI
  , screenSize :: VecInt
  , harvestManager :: HarvestManager
  , effectObjects :: [] EffectObject
  }


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
        { env = env'
        , sceneMeta = initialSceneMetaData
        , actorActList = []
        , player = initialPlayer screenSize'
        , background = initialBackground
        , meteorManager = initialMeteorManager
        , harvestManager = initialHarvestManager screenSize'
        , infoUI = infoUI'
        , screenSize = screenSize'
        , effectObjects = []
        }

  op scene


initPlaying :: Scene -> Scene
initPlaying s =
  let size = screenSize s
      pr = sceneMeta s^.playingRecord
      pr' = pr& currScore.~0 & currLevel.~1
  in s
  { sceneMeta = (sceneMeta s) {_playingRecord = pr'}
  , player = initialPlayer size
  , background = initialBackground
  , meteorManager = initialMeteorManager
  , harvestManager = initialHarvestManager size
  }


isSceneState :: SceneState -> Scene -> Bool
isSceneState state s = state == sceneMeta s^.sceneState


justCropped :: Scene -> Harvest -> Bool
justCropped s harv = whenCropped harv == (-1 + sceneMeta s^.sceneFrame)


isHitStopping :: Scene -> Bool
isHitStopping s =
  let ps = playerState $ player s
  in case ps of
      HitStopping _ -> True
      _ -> False
