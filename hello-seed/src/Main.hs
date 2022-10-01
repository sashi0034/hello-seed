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
import Data.StateVar


data World = World
  { exiting :: Bool
  , frame   :: Int
  , playerPos :: Pos
  }




data Size = Size
  { width :: Int
  , height :: Int
  }

data Pos = Pos
  { x :: Int
  , y :: Int
  }

data InputIntent
  = Idle
  | Left
  | Right
  | Up
  | Down
  | Quit


initialApp :: World
initialApp = World
  { exiting = False
  , frame   = 0
  , playerPos = Pos 0 0
  }


main :: IO ()
main = SDLWrapper.withSDL $ SDLWrapper.withSDLImage $ do
  SDLWrapper.setHintQuality
  SDLWrapper.withWindow "Haskell Test" (640, 480) $ \w ->
    SDLWrapper.withRenderer w $ \r -> do
      SDLWrapper.withTexture r "./assets/blobwob_24x24.png" $ \t -> do
        let doRender = renderApp r t
        runApp (appLoop doRender) initialApp


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadIO m) => (World -> m ())-> World -> m World
appLoop r a
  = updateApp a <$> pollIntents
  >>= \a' -> a' <$ r a'


updateApp :: World -> [InputIntent] -> World
updateApp a = stepFrame . foldl' applyIntent a


pollIntents :: (MonadIO m) => m [InputIntent]
pollIntents = eventToIntent `fmap2` SDL.pollEvents

  where
    fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> InputIntent
eventToIntent (SDL.Event _t SDL.QuitEvent) = Main.Quit
eventToIntent (SDL.Event _t (SDL.KeyboardEvent k)) = keyEventToIntent k
eventToIntent _                            = Main.Idle


keyEventToIntent :: SDL.KeyboardEventData -> InputIntent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of

    SDL.KeycodeEscape -> Quit

    SDL.KeycodeA -> Main.Left
    SDL.KeycodeD -> Main.Right
    SDL.KeycodeW -> Main.Up
    SDL.KeycodeS -> Main.Down

    _            -> Idle


keyEventToIntent _ = Idle

applyIntent :: World -> InputIntent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a Main.Left = a { playerPos = Pos (-2 + (x $ playerPos a)) (y $ playerPos a) }
applyIntent a Main.Right = a { playerPos = Pos (2 + (x $ playerPos a)) (y $ playerPos a) }
applyIntent a Main.Up = a { playerPos = Pos (x $ playerPos a) (-2 + (y $ playerPos a)) }
applyIntent a Main.Down = a { playerPos = Pos (x $ playerPos a) (2 + (y $ playerPos a)) }
applyIntent a Idle = a


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

    --renderingSize = SDLWrapper.mkRect 0 0 (fromIntegral $ width cellSize * cellScale) (fromIntegral $ height cellSize * cellScale)
    --windowSize = SDLWrapper.mkRect 0 0 640 480
    dest = fromIntegral <$> SDLWrapper.mkRect (x $ playerPos a) (y $ playerPos a) (cellScale * width cellSize) (cellScale * height cellSize)


-- centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
-- centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
--   = SDL.Rectangle p iz

--   where
--     p = SDL.P $ op + (oz - iz) / 2
