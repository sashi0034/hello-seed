module MainScene.EffectObject where
import Vec (VecF)
import Types


type EffAccel = VecF
type EffPos = VecF
type EffVec = VecF
type DelayCount = FrameCount


data EffectObject = 
    OvalElem FrameCount EffPos EffVec DelayCount
  | BlobElem FrameCount EffPos EffVec




