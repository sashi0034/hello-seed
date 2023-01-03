{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDLWrapper
import InputIntent

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.StateVar
import ImageRsc
import InputState ( readInput, InputState(intents) )
import qualified Scene.SceneAct as SceneAct
import Vec (Vec(Vec), getY, getX)
import Data.Time
import Control.Concurrent (threadDelay)
import SDL.Font ()
import qualified FontRsc as ImageRsc
import qualified Scene.Scene as Scene
import Scene.Scene
import Scene.SceneAct (setupScene)
import Control.Lens
import SoundRsc (SoundRsc(SoundRsc), withSoundRsc)



main :: IO ()
main = 
  SDLWrapper.withSDL $ 
  SDLWrapper.withSDLImage $ 
  SDLWrapper.withSDLFont $ 
  SDLWrapper.withSDLMixer $ do
  SDLWrapper.setHintQuality
  let initialWindowSize = Vec 1280 (720 :: Int)

  SDLWrapper.withWindow "Haskell Test" (getX initialWindowSize, getY initialWindowSize) $ \w ->
    SDLWrapper.withRenderer w $ \r ->
    ImageRsc.withImageRsc r $ \imageRsc' ->
    ImageRsc.withFontRsc $ \fontRsc' ->
    SoundRsc.withSoundRsc $ \soundRsc' ->
    Scene.withScene
        (Scene.initialEnv w r imageRsc' fontRsc' soundRsc' initialWindowSize)
        initialWindowSize
      $ \s -> do

    runApp loopApp (setupScene s)


runApp :: (Monad m) => (Scene -> m Scene) -> Scene -> m ()
runApp f = repeatUntil f (exiting . _sceneEnv)


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


loopApp :: (MonadIO m) => Scene -> m Scene
loopApp s = do
  controlFpsInApp $ do
    input' <- InputState.readInput
    refreshApp s { _sceneEnv = (s^.env){ input = input' } }


controlFpsInApp :: MonadIO m => m Scene -> m Scene
controlFpsInApp process = do
  loopStartTime <- liftIO getCurrentTime

  s' <- process

  loopEndTime <- liftIO getCurrentTime
  
  let idealFps = fromIntegral $ currentBaseFps $ s' ^. env
  let idealDuration = 1000 * 1000 * ((1 :: Float) / idealFps)

  let deltaTime = diffUTCTime loopEndTime loopStartTime
  let realDuration = 1000 * 1000 * fromRational (toRational deltaTime)
  let sleepDuration = idealDuration - realDuration

  liftIO $ threadDelay $ floor sleepDuration

  return s'



applyIntents :: (MonadIO  m) => Environment -> [InputIntent] -> m Environment
applyIntents a [] = return a
applyIntents a (intent:list) = do
  let a' = applyIntent a intent
  applyIntents a' list


applyIntent :: Environment -> InputIntent -> Environment
applyIntent environ InputIntent.Quit = environ { exiting = True }
applyIntent environ _ = environ


refreshApp :: (MonadIO m) => Scene -> m Scene
refreshApp s = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= SDL.V4 100 100 100 255

  SDL.clear r
  
  env' <- applyIntents (s^.env) $ intents (input $ s^.env)
  s' <- SceneAct.refreshScene s {_sceneEnv = env'}

  SDL.present r

  return s'

  where
    r = renderer $ s^.env

