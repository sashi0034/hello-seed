module MainScene.EffectObject where
import Vec (VecF)


type EffAccel = VecF
type EffPos = VecF
type EffVec = VecF
type FrameCount = Int
type DelayCount = FrameCount


data EffectObject = 
    OvalElem FrameCount EffPos EffVec DelayCount
  -- | Some FrameCount




