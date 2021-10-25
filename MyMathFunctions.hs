module MyMathFunctions
(
  extendedEuclidean
) where

extendedEuclidean :: Int -> Int -> (Int, Int, Int)
extendedEuclidean e m =
  if m == 0
  then (e, 1, 0)
  else (d, b, a - (e `div` m) * b)
  where
    (d, a, b) = extendedEuclidean m (e `mod` m)

