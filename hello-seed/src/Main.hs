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
import qualified Scene.SceneBehavior as SceneBehavior
import Vec (Vec(Vec), getY, getX)
import Data.Time
import Control.Concurrent (threadDelay)
import SDL.Font ()
import qualified FontRsc as ImageRsc
import qualified Scene.Scene as Scene
import Scene.Scene
import Scene.SceneBehavior (setupScene)



main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ SDLWrapper.withSDLFont $ do
  SDLWrapper.setHintQuality
  let initialWindowSize = Vec 1280 (720 :: Int)

  SDLWrapper.withWindow "Haskell Test" (getX initialWindowSize, getY initialWindowSize) $ \w ->
    SDLWrapper.withRenderer w $ \r ->
    ImageRsc.loadImageRsc r $ \imageRsc' ->
    ImageRsc.loadFontRsc $ \fontRsc' ->
    Scene.withScene
        (Scene.initialEnv w r imageRsc' fontRsc' initialWindowSize)
        initialWindowSize
      $ \s -> do

    runApp loopApp (setupScene s)


runApp :: (Monad m) => (Scene -> m Scene) -> Scene -> m ()
runApp f = repeatUntil f (exiting . env)


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


loopApp :: (MonadIO m) => Scene -> m Scene
loopApp s = do
  controlFpsInApp $ do
    input' <- InputState.readInput
    refreshApp s { env = (env s){ input = input' } }


controlFpsInApp :: MonadIO m => m Scene -> m Scene
controlFpsInApp process = do
  loopStartTime <- liftIO getCurrentTime

  s' <- process

  loopEndTime <- liftIO getCurrentTime
  
  let idealFps = fromIntegral $ currentBaseFps $ env s'
  let idealDuration = 1000 * 1000 * ((1 :: Float) / idealFps)

  let deltaTime = diffUTCTime loopEndTime loopStartTime
  let realDuration = 1000 * 1000 * fromRational (toRational deltaTime)
  let sleepDuration = idealDuration - realDuration

  liftIO $ threadDelay $ floor sleepDuration

  return s'



applyIntents :: (MonadIO  m) => Environment -> [InputIntent] -> m Environment
applyIntents a [] = return a
applyIntents a (intent:intents) = do
  let a' = applyIntent a intent
  applyIntents a' intents


applyIntent :: Environment -> InputIntent -> Environment
applyIntent env InputIntent.Quit = env { exiting = True }
applyIntent env _ = env


refreshApp :: (MonadIO m) => Scene -> m Scene
refreshApp s = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= SDL.V4 100 100 100 255

  SDL.clear r
  
  env' <- applyIntents (env s) $ intents (input $ env s)
  s' <- SceneBehavior.refreshScene s {env = env'}

  SDL.present r

  return s'

  where
    r = renderer $ env s

