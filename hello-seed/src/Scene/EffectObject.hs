{-# LANGUAGE TemplateHaskell #-}
module Scene.EffectObject where
import Vec
import Types
import ImageRsc (ImageGetter)
import Rendering
import Control.Lens (makeLenses)
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Operators


type EffAccel = VecF
type EffPos = VecF
type EffVec = VecF
type DelayFrame = FrameCount
type LifetimeFrame = FrameCount
type ScrapCount = Int


data EffectObject =
    OvalElem FrameCount EffPos EffVec DelayFrame
  | BlobElem FrameCount EffPos EffVec
  | ScrapTexture EffectParam ImageGetter SrcRect ScrapCount


data EffectParam = EffectParam
    { _effFrame :: FrameCount
    , _effLifetime:: LifetimeFrame
    , _effPos :: EffPos
    , _effVec :: EffVec
    } deriving (Show)


makeLenses ''EffectParam


initialEffectParam :: FrameCount -> EffPos -> EffVec -> EffectParam
initialEffectParam lifetime pos vec = EffectParam
  { _effFrame = 0
  , _effLifetime = lifetime
  , _effPos = pos
  , _effVec = vec
  }


updateEffectParam :: EffectParam -> EffectParam
updateEffectParam param = param
      & effFrame %~ (+1)
      & effPos %~ (~+ param^.effVec)


isAliveEffectParam :: EffectParam -> Bool
isAliveEffectParam param = param^.effFrame < param^.effLifetime



makeScrapEffect1 = makeScrapEffect 1


makeScrapEffect scrapCount lifeTime speed center (SrcRect originStart originSize) image =
  let makeEff vel srcRect =
        ScrapTexture
          (initialEffectParam (lifeTime + delay) (toVecF srcStart ~+ center) vel)
          image
          srcRect
          scrapCount
        where (SrcRect srcStart _) = srcRect
              delay = 0 -- 2 * getY srcStart

  in [ makeEff (Vec (-speed) speed) (SrcRect (originStart ~+ Vec 0 cellH) cellSize)
  , makeEff (Vec (-speed) (-speed)) (SrcRect (originStart ~+ Vec 0 0) cellSize)
  , makeEff (Vec speed (-speed)) (SrcRect (originStart ~+ Vec cellW 0) cellSize)
  , makeEff (Vec speed speed) (SrcRect (originStart ~+ Vec cellW cellH) cellSize)
  ] where
      cellSize = originSize `divVec` 2
      (Vec cellW cellH) = cellSize