
module Vec where


data Vec a = Vec a a deriving ( Show )


(~+) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~+ (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)


(~-) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~- (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)


(~*) :: (Num a) => Vec a -> a -> Vec a
(Vec x1 y1) ~* times = Vec (x1 * times) (y1 * times)


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


vecZero :: (Num a) => Vec a
vecZero = Vec 0 0

