{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Scene.Scene where
import Vec
import Scene.Player
import Scene.Background
import Scene.MeteorManager (MeteorManager, initialMeteorManager)
import Scene.InfoUI
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import Control.Monad.Cont
import Scene.HarvestManager (HarvestManager, initialHarvestManager, Harvest (whenCropped))
import Scene.EffectObject (EffectObject)
import Types (FrameCount)
import qualified SDL
import ImageRsc
import FontRsc
import InputState


data SceneState = Title | Playing deriving (Eq)

data PlayingRecord = PlayingRecord
  { currScore :: Int
  , highScore :: Int
  , currLevel :: Int
  }


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


data Scene = Scene
  { env :: Environment
  , actorActList :: [] ActorAct
  -- TODO: scene~をMetaにする
  , sceneState :: SceneState
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


withScene :: MonadIO m => Environment -> VecInt -> (forall (m :: * -> *). (MonadIO m) => Scene -> m ()) -> m()
withScene env' screenSize' op =  (`runContT` return) $ do
  infoUI' <- ContT initialInfoUI

  let scene = Scene
        { env = env'
        , actorActList = []
        , sceneState = Title
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


initPlaying :: Scene -> Scene
initPlaying s =
  let size = screenSize s
  in s
  { playingRecord = (playingRecord s){currScore=0, currLevel=1}
  , player = initialPlayer size
  , background = initialBackground
  , meteorManager = initialMeteorManager
  , harvestManager = initialHarvestManager size
  }


activeInSceneWhen :: SceneState -> Scene -> Bool
activeInSceneWhen state s = state == sceneState s


justCropped :: Scene -> Harvest -> Bool
justCropped s harv = whenCropped harv == (-1 + sceneFrame s)


isHitStopping :: Scene -> Bool
isHitStopping s =
  let ps = playerState $ player s
  in case ps of
      HitStopping _ -> True
      _ -> False