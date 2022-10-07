{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDLWrapper
import InputIntent
import Vec
import World

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.StateVar
import ImageRsc
import MainScene.MainScene (MainScene (player))
import MainScene.Player


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
  intents <- InputIntent.pollIntents
  a' <- updateApp a intents
  r a'
  return a'



updateApp ::(MonadIO  m) =>  World -> [InputIntent] -> m World
updateApp a intents = do 
  a' <- applyIntents a intents
  stepFrame a'


applyIntents :: (MonadIO  m) => World -> [InputIntent] -> m World
applyIntents a [] = return a
applyIntents a (intent:intents) = do 
  let a' = applyIntent a intent
  applyIntents a' intents


applyIntent :: World -> InputIntent -> World
applyIntent world InputIntent.Quit = world { exiting = True }
applyIntent world InputIntent.Left = movePlayer world $ Vec.Pos (-1) (0)
applyIntent world InputIntent.Right = movePlayer world $ Vec.Pos (1) (0)
applyIntent world InputIntent.Up = movePlayer world $ Vec.Pos (0) (-1)
applyIntent world InputIntent.Down = movePlayer world $ Vec.Pos (0) (1)
applyIntent world InputIntent.Idle = world


movePlayer :: World -> Vec.Pos -> World
movePlayer world deltaPos = world { scene = scene' {player = player'{pos = Pos (x deltaPos + x currPos) (y deltaPos + y currPos)} } }
  where
    scene' = scene world
    player' = player scene'
    currPos = pos player'


stepFrame :: (MonadIO m) => World -> m World
stepFrame a = return a { frame = frame a + 1 }


renderApp :: (MonadIO m) => SDL.Renderer -> ImageRsc -> World -> m ()
renderApp r imageRsc world = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= SDL.V4 100 100 100 255

  SDL.clear r
  SDL.copy r (blobwob_24x24 imageRsc) (Just mask) (Just dest)
  SDL.present r

  where
    frameDuration = 200
    numFrame = 10
    cellSize = Size 24 24
    cellScale = 3
    srcX = (frame world `div` frameDuration) `mod` numFrame
    mask = fromIntegral <$> SDLWrapper.mkRect (srcX * width cellSize) 0 (width cellSize) (height cellSize)
    dest = fromIntegral <$> SDLWrapper.mkRect (x playerPos) (y playerPos) (cellScale * width cellSize) (cellScale * height cellSize)
      where
        scene' = scene world
        player' = player scene'
        playerPos = pos player'
