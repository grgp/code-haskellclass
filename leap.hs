isLeapYear :: Int -> Bool
isLeapYear t =
  (mod t 4 == 0) &&
    ( (mod t 400 == 0) || (mod t 100 /= 0) )