{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDLWrapper
import InputIntent
import Vector
import World

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.StateVar
import ImageRsc
import MainScene.MainScene (MainScene (player, MainScene))
import MainScene.MainSceneBehavior
import InputState (InputState (intents, InputState), readInput)
import qualified MainScene.MainSceneBehavior as MainSceneBehavior


main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ do
  SDLWrapper.setHintQuality
  SDLWrapper.withWindow "Haskell Test" (640, 480) $ \w ->
    SDLWrapper.withRenderer w $ \r -> do
      ImageRsc.loadImageRsc r $ \imageRsc -> do
        let app = World.initialWorld
        runApp (appLoop $ renderApp r imageRsc) app


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadIO m) => (World -> m ())-> World -> m World
appLoop r a = do
  input <- InputState.readInput
  a' <- updateApp a input
  r a'
  return a'



updateApp ::(MonadIO  m) =>  World -> InputState -> m World
updateApp world input = do
  world' <- applyIntents world $ intents input
  world'' <- stepFrame world'
  scene' <- MainSceneBehavior.updateMainScene world'' input
  return world'' {scene=scene'}
  


applyIntents :: (MonadIO  m) => World -> [InputIntent] -> m World
applyIntents a [] = return a
applyIntents a (intent:intents) = do 
  let a' = applyIntent a intent
  applyIntents a' intents


applyIntent :: World -> InputIntent -> World
applyIntent world InputIntent.Quit = world { exiting = True }
applyIntent world _ = world


stepFrame :: (MonadIO m) => World -> m World
stepFrame a = return a { frame = frame a + 1 }


renderApp :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderApp r imageRsc world = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= SDL.V4 100 100 100 255

  SDL.clear r
  MainSceneBehavior.renderMainScene r imageRsc world
  SDL.present r
