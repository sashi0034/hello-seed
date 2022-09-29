{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified SDLWrapper

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable   (foldl')


data World = World
  { exiting :: Bool
  , frame   :: Int
  }


data Intent = Idle | Quit


initialApp :: World
initialApp = World
  { exiting = False
  , frame   = 0
  }


main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ do
  SDLWrapper.setHintQuality
  SDLWrapper.withWindow "Lesson 14" (640, 480) $ \w ->
    SDLWrapper.withRenderer w $ \r -> do
      t <- SDL.Image.loadTexture r "./assets/walk.png"
      let doRender = renderApp r t
      runApp (appLoop doRender) initialApp
      SDL.destroyTexture t


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadIO m) => (World -> m ())-> World -> m World
appLoop r a
  = updateApp a <$> pollIntents
  >>= \a' -> a' <$ r a'


updateApp :: World -> [Intent] -> World
updateApp a = stepFrame . foldl' applyIntent a


pollIntents :: (MonadIO m) => m [Intent]
pollIntents = eventToIntent `fmap2` SDL.pollEvents

  where
    fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> Intent
eventToIntent (SDL.Event _t SDL.QuitEvent) = Quit
eventToIntent _                            = Idle


applyIntent :: World -> Intent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a Idle = a


stepFrame :: World -> World
stepFrame a = a { frame = frame a + 1 }


renderApp :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> World -> m ()
renderApp r t a = do
  SDL.clear r
  SDL.copy r t (Just mask) (Just pos)
  SDL.present r

  where
    x = (frame a `div` 8) `mod` 8
    mask = fromIntegral <$> SDLWrapper.mkRect (x * 48) 0 48 48

    s = SDLWrapper.mkRect 0 0 192 (192 :: Double)
    w = SDLWrapper.mkRect 0 0 640 480
    pos = floor <$> centerWithin s w


centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz

  where
    p = SDL.P $ op + (oz - iz) / 2
