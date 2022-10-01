{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module InputIntent
  ( InputIntent(..)
  , pollIntents
  ) where

import qualified SDL
import Control.Monad.IO.Class


data InputIntent
  = Idle
  | Left
  | Right
  | Up
  | Down
  | Quit


pollIntents :: (MonadIO m) => m [InputIntent]
pollIntents = eventToIntent `fmap2` SDL.pollEvents

  where
    fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> InputIntent
eventToIntent (SDL.Event _t SDL.QuitEvent) = InputIntent.Quit
eventToIntent (SDL.Event _t (SDL.KeyboardEvent k)) = keyEventToIntent k
eventToIntent _                            = InputIntent.Idle


keyEventToIntent :: SDL.KeyboardEventData -> InputIntent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of

    SDL.KeycodeEscape -> InputIntent.Quit

    SDL.KeycodeA -> InputIntent.Left
    SDL.KeycodeD -> InputIntent.Right
    SDL.KeycodeW -> InputIntent.Up
    SDL.KeycodeS -> InputIntent.Down

    _            -> InputIntent.Idle
keyEventToIntent _ = InputIntent.Idle

