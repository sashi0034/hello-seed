module CollisionUtil where
import Vec (VecF, Vec (Vec))


type RectSize = VecF
data ColRect = ColRect VecF RectSize


hitRectRect :: ColRect -> ColRect -> Bool
hitRectRect (ColRect (Vec x1 y1) (Vec w1 h1)) (ColRect (Vec x2 y2)(Vec w2 h2)) =
  let
    hitX = abs (x2 + w2 / 2 - (x1 + w1 / 2)) < (w1 + w2) /2
    hitY = abs (y2 + h2 / 2 - (y1 + h1 / 2)) < (h1 + h2) /2
  in hitX && hitY



