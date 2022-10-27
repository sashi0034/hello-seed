{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDLWrapper
import InputIntent
import World

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.StateVar
import ImageRsc
import InputState ( readInput, InputState(intents) )
import qualified MainScene.MainSceneBehavior as MainSceneBehavior
import Vec (Vec(Vec), getY, getX)
import Data.Time
import Control.Concurrent (threadDelay)
import SDL.Font ()
import qualified FontRsc as ImageRsc
import qualified MainScene.MainScene as MainScene



main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ SDLWrapper.withSDLFont $ do
  SDLWrapper.setHintQuality
  let initialWindowSize = Vec 1280 (720 :: Int)
  SDLWrapper.withWindow "Haskell Test" (getX initialWindowSize, getY initialWindowSize) $ \w ->
    SDLWrapper.withRenderer w $ \r -> do
      ImageRsc.loadImageRsc r $ \imageRsc' -> do
        ImageRsc.loadFontRsc $ \fontRsc' -> do
          MainScene.withMainScene initialWindowSize $ \scene' -> do

            let world = World.initialWorld w r imageRsc' fontRsc' initialWindowSize scene'
            runApp loopApp world


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


loopApp :: (MonadIO m) => World -> m World
loopApp world = do
  controlFpsInApp world $ do
    input' <- InputState.readInput
    world' <- updateApp world {input = input'}
    renderApp world'
    return world'


controlFpsInApp :: MonadIO m => World -> (m World) -> m World
controlFpsInApp world process = do
  loopStartTime <- liftIO getCurrentTime
  
  world' <- process

  loopEndTime <- liftIO getCurrentTime

  let deltaTime = diffUTCTime loopEndTime loopStartTime
  let realDuration = 1000 * 1000 * (fromRational $ toRational deltaTime)
  let sleepDuration = idealDuration - realDuration
  
  liftIO $ threadDelay $ floor sleepDuration

  return world'

  where
    idealFps = fromIntegral $ baseFps world
    idealDuration = 1000 * 1000 * ((1 :: Float) / idealFps)



updateApp ::(MonadIO  m) =>  World -> m World
updateApp world = do
  world' <- applyIntents world $ intents (input world)
  scene' <- MainSceneBehavior.updateMainScene world'
  return world' {scene=scene'}



applyIntents :: (MonadIO  m) => World -> [InputIntent] -> m World
applyIntents a [] = return a
applyIntents a (intent:intents) = do
  let a' = applyIntent a intent
  applyIntents a' intents


applyIntent :: World -> InputIntent -> World
applyIntent world InputIntent.Quit = world { exiting = True }
applyIntent world _ = world


renderApp :: (MonadIO m) => World -> m ()
renderApp world = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= SDL.V4 100 100 100 255

  SDL.clear r
  MainSceneBehavior.renderMainScene world
  SDL.present r

  where
    r = renderer world

