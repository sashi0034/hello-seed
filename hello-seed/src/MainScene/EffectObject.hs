module MainScene.EffectObject where
import Vec (VecF)


type EffAccel = VecF
type EffPos = VecF
type EffVec = VecF
type FrameCount = Int


data EffectObject = 
    OvalGen FrameCount EffPos
  | OvalElem FrameCount EffPos EffVec




