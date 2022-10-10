
module InputState
  ( InputState(..)
  , readInput
  , MouseState(..)
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
  { mousePos :: Pos
  }


readInput :: (MonadIO m) => m InputState
readInput = do
  mousePos <- SDL.Input.getAbsoluteMouseLocation
  let mousePos' = convertP mousePos
  let mousePos'' = convertV2 mousePos'

  intents' <- InputIntent.pollIntents

  return InputState 
    { mouse = MouseState
        { mousePos = mousePos'' 
        }
    , intents = intents'
    }



convertP (LA.P p) = p

convertV2 :: SDL.V2 CInt -> Pos
convertV2 (SDL.V2 x y) = Pos (fromIntegral x) (fromIntegral y)
