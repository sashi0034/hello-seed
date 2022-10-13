
module Vec where


data Vec a = Vec a a deriving ( Show )


(~+) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~+ (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)


(~-) :: (Num a) => Vec a -> Vec a -> Vec a
(Vec x1 y1) ~- (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)


type VecF = Vec Float


type VecInt = Vec Int


toVecF :: (Integral a1, Num a2) => Vec a1 -> Vec a2
toVecF v = Vec (fromIntegral $ getX v) (fromIntegral $ getY v)


getX :: Vec a -> a
getX (Vec x _) = x


getY :: Vec a -> a
getY (Vec _ y) = y

