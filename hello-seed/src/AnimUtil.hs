module AnimUtil where


calcAnimFrameIndex :: Int -> Int -> Int -> Int
calcAnimFrameIndex numFrame frameDuration count =
  (count `div` frameDuration) `mod` numFrame

