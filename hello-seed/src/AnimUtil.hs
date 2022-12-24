{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module AnimUtil
( calcAnimFrameIndex
, degToRad
, radToDeg
, convertRGB
, RangeF(..)
, valueWithEaseBegin
) where
import Ease
import Types

calcAnimFrameIndex :: Int -> Int -> Int -> Int
calcAnimFrameIndex numFrame frameDuration count =
  count `div` frameDuration `mod` numFrame


class DegRadConverter a where
  degToRad :: a -> Float
  radToDeg :: Float -> a

instance DegRadConverter Int where
  degToRad :: Int -> Float
  degToRad deg = fromIntegral deg * pi / 180
  radToDeg :: Float -> Int
  radToDeg rad = floor $ rad * 180 / pi

instance DegRadConverter Float where
  degToRad :: Float -> Float
  degToRad deg = deg * pi / 180
  radToDeg :: Float -> Float
  radToDeg rad = rad * 180 / pi



convertRGB :: (Integral a1, Integral a2, Integral a3, Num t1) =>
  (t1 -> t1 -> t1 -> t1 -> t4) -> a1 -> a2 -> a3 -> t4
convertRGB v r g b = v (fromIntegral r) (fromIntegral g) (fromIntegral b) 255


data RangeF = RangeF Float Float


valueWithEaseBegin :: Ease Float -> RangeF -> Int -> FrameCount -> Float
valueWithEaseBegin ease (RangeF startValue endValue) easeDuration frame  =
  let easeRate = 1 - fromIntegral frame / fromIntegral easeDuration
      easedDelta = (endValue - startValue) * ease easeRate
  in if frame < easeDuration
    then startValue + easedDelta
    else endValue

