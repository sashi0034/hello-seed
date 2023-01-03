
module InputState
  ( InputState(..)
  , readInput
  , MouseState(..)
  , noInput
  ) where

import qualified SDL.Input
import InputIntent
import Vec
import Control.Monad.IO.Class
import qualified SDL
import Linear.Affine as LA
import Foreign.C (CInt)


data InputState = InputState
  { intents :: [InputIntent],
    mouse :: MouseState
  }

data MouseState = MouseState
  { mousePos :: VecInt
  , mouseButton :: SDL.MouseButton -> Bool
  }


noInput :: InputState
noInput = InputState
  { intents = []
  , mouse = MouseState
      { mousePos=vecZero
      , mouseButton= const False }
  }


readInput :: (MonadIO m) => VecInt -> m InputState
readInput screenOffset = do
  pos <- SDL.Input.getAbsoluteMouseLocation
  let pos' = convertV2 $ convertP pos
  
  button <- SDL.Input.getMouseButtons

  intents' <- InputIntent.pollIntents

  return InputState
    { mouse = MouseState
        { mousePos = pos' ~- screenOffset
        , mouseButton = button
        }
    , intents = intents'
    }



convertP :: Point f a -> f a
convertP (LA.P p) = p

convertV2 :: SDL.V2 CInt -> VecInt
convertV2 (SDL.V2 x y) = Vec (fromIntegral x) (fromIntegral y)
