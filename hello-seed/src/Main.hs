{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified SDLWrapper
import InputIntent
import Vec
import World

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable   (foldl')
import Data.StateVar


main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ do
  SDLWrapper.setHintQuality
  SDLWrapper.withWindow "Haskell Test" (640, 480) $ \w ->
    SDLWrapper.withRenderer w $ \r -> do
      SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \t -> do            
        runApp (appLoop $ renderApp r t) World.initialApp


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadIO m) => (World -> m ())-> World -> m World
appLoop r a
  = updateApp a <$> InputIntent.pollIntents
  >>= \a' -> a' <$ r a'


updateApp :: World -> [InputIntent] -> World
updateApp a = stepFrame . foldl' applyIntent a


applyIntent :: World -> InputIntent -> World
applyIntent a InputIntent.Quit = a { exiting = True }
applyIntent a InputIntent.Left = a { playerPos = Pos (-2 + (x $ playerPos a)) (y $ playerPos a) }
applyIntent a InputIntent.Right = a { playerPos = Pos (2 + (x $ playerPos a)) (y $ playerPos a) }
applyIntent a InputIntent.Up = a { playerPos = Pos (x $ playerPos a) (-2 + (y $ playerPos a)) }
applyIntent a InputIntent.Down = a { playerPos = Pos (x $ playerPos a) (2 + (y $ playerPos a)) }
applyIntent a InputIntent.Idle = a


stepFrame :: World -> World
stepFrame a = a { frame = frame a + 1 }


renderApp :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> World -> m ()
renderApp r t a = do
  let renderColor = SDL.rendererDrawColor r
  renderColor $= (SDL.V4 100 100 100 255)
  
  SDL.clear r
  SDL.copy r t (Just mask) (Just dest)
  SDL.present r

  where
    frameDuration = 200
    numFrame = 10
    cellSize = Size 24 24
    cellScale = 3
    srcX = (frame a `div` frameDuration) `mod` numFrame
    mask = fromIntegral <$> SDLWrapper.mkRect (srcX * width cellSize) 0 (width cellSize) (height cellSize)
    dest = fromIntegral <$> SDLWrapper.mkRect (x $ playerPos a) (y $ playerPos a) (cellScale * width cellSize) (cellScale * height cellSize)

