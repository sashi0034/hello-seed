module Scene.Background
  ( Background(..)
  , BgNextInfo(..)
  , initialBackground) where
import ImageRsc (ImageRsc (bg_a), ImageGetter)
import Types (FrameCount)


data BgNextInfo = BgNextInfo ImageGetter FrameCount


data Background = Background
  { bgAnimCount :: Int
  , bgCurrImage :: ImageGetter 
  , bgNextInfo :: Maybe BgNextInfo
  }


initialBackground :: Background
initialBackground = Background
  { bgAnimCount = 0
  , bgCurrImage = bg_a
  , bgNextInfo = Nothing
  }

