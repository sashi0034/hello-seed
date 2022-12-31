module Scene.Background
  ( Background(..)
  , BgNextInfo(..)
  , initialBackground
  , getBgImageByLevel
  ) where
import ImageRsc (ImageRsc (bg_a, bg_b, bg_c, bg_d), ImageGetter)
import Types (FrameCount)
import qualified ConstParam


data BgNextInfo = BgNextInfo ImageGetter FrameCount


data Background = Background
  { bgAnimCount :: Int
  , bgCurrImage :: ImageGetter 
  , bgNextInfo :: Maybe BgNextInfo
  }


initialBackground :: Background
initialBackground = Background
  { bgAnimCount = 0
  , bgCurrImage = getBgImageByLevel ConstParam.initialLevel
  , bgNextInfo = Nothing
  }


getBgImageByLevel :: Int -> ImageGetter
getBgImageByLevel level = case level `mod` 4 of
  1 -> bg_a
  2 -> bg_b
  3 -> bg_c
  0 -> bg_d
  _ -> undefined
