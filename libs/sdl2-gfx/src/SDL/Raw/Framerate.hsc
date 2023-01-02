{-|

Module      : SDL.Raw.Framerate
Copyright   : (c) 2015 Siniša Biđin, 2021 Daniel Firth
License     : MIT
Maintainer  : sinisa@bidin.eu, dan.firth@homotopic.tech
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the framerate management
functionality from @SDL2_framerate.h@.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Framerate
  ( Manager(..)
  , init
  , framerateDelay
  , setFramerate
  , pattern FPS_DEFAULT
  , pattern FPS_LOWER_LIMIT
  , pattern FPS_UPPER_LIMIT
  , getFramerate
  , getFramecount
  ) where

#include "SDL2_framerate.h"

import Foreign.C.Types  (CFloat(..), CInt(..))
import Foreign.Ptr      (Ptr)
import Foreign.Storable (Storable(..))
import Data.Word        (Word32)
import Prelude   hiding (init)
import SDL.Raw.Helper   (liftF)

pattern FPS_DEFAULT     = (#const FPS_DEFAULT)
pattern FPS_LOWER_LIMIT = (#const FPS_LOWER_LIMIT)
pattern FPS_UPPER_LIMIT = (#const FPS_UPPER_LIMIT)

data Manager = Manager
  { frameCount :: Word32
  , rateTicks  :: CFloat
  , baseTicks  :: Word32
  , lastTicks  :: Word32
  , rate       :: Word32
  } deriving stock (Eq, Show, Read)

instance Storable Manager where
  alignment = sizeOf
  sizeOf _ = (#size FPSmanager)

  peek ptr =
    Manager
      <$> (#peek FPSmanager, framecount) ptr
      <*> (#peek FPSmanager, rateticks)  ptr
      <*> (#peek FPSmanager, baseticks)  ptr
      <*> (#peek FPSmanager, lastticks)  ptr
      <*> (#peek FPSmanager, rate)       ptr

  poke ptr (Manager {..}) = do
    (#poke FPSmanager, framecount) ptr frameCount
    (#poke FPSmanager, rateticks)  ptr rateTicks
    (#poke FPSmanager, baseticks)  ptr baseTicks
    (#poke FPSmanager, lastticks)  ptr lastTicks
    (#poke FPSmanager, rate)       ptr rate

liftF "init" "SDL_initFramerate"
  [t|Ptr Manager -> IO ()|]

liftF "getFramecount" "SDL_getFramecount"
  [t|Ptr Manager -> IO CInt|]

liftF "framerateDelay" "SDL_framerateDelay"
  [t|Ptr Manager -> IO Word32|]

liftF "getFramerate" "SDL_getFramerate"
  [t|Ptr Manager -> IO CInt|]

liftF "setFramerate" "SDL_setFramerate"
  [t|Ptr Manager -> Word32 -> IO CInt|]
