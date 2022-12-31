{-# LANGUAGE InstanceSigs #-}

module Vec where


data Vec a = Vec a a deriving ( Show )


(~+) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~+ (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)


(~-) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~- (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)


(~*) :: (Num a) => Vec a -> a -> Vec a
(Vec x1 y1) ~* times = Vec (x1 * times) (y1 * times)


divVec :: Integral a => Vec a -> a -> Vec a
(Vec x1 y1) `divVec` d = Vec (x1 `div` d) (y1 `div` d)


infixl 3 ~+
infixl 3 ~-
infixl 5 ~*
infixl 4 `divVec`


type VecF = Vec Float


type VecInt = Vec Int


toVecF :: VecInt -> VecF
toVecF v = Vec (fromIntegral $ getX v) (fromIntegral $ getY v)


toVecInt :: VecF -> VecInt
toVecInt v = Vec (floor $ getX v) (floor $ getY v)


getX :: Vec a -> a
getX (Vec x _) = x


getY :: Vec a -> a
getY (Vec _ y) = y


sqrMagnitude :: Num a => Vec a -> a
sqrMagnitude (Vec x y) = x * x + y * y


normalize :: VecF -> VecF
normalize (Vec x y) = Vec (x / size) (y / size)
  where
    size = sqrt $ sqrMagnitude $ Vec x y


radOfVecF :: VecF -> Float
radOfVecF (Vec x y) = atan2 y x


degOfVecF :: VecF -> Float
degOfVecF v = 180 * radOfVecF v / pi


vecFromRad :: Float -> VecF
vecFromRad rad = Vec (cos rad) (sin rad)


class VecFromDeg a where
  vecFromDeg :: a -> VecF
instance VecFromDeg Int where
  vecFromDeg :: Int -> VecF
  vecFromDeg deg = vecFromRad ((fromIntegral deg / 180) * pi)
instance VecFromDeg Float where
  vecFromDeg :: Float -> VecF
  vecFromDeg deg = vecFromRad (deg / (180 :: Float) * pi)


vecZero :: (Num a) => Vec a
vecZero = Vec 0 0


vecUnit :: (Num a) => Vec a
vecUnit = Vec 1 1


convertVecInt :: (Num t1) => (t1 -> t1 -> t2) -> VecInt -> t2
convertVecInt f v = f (fromIntegral $ getX v) (fromIntegral $ getY v)
