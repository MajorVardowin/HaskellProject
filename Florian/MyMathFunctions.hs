import System.IO (getLine)
import System.Random (randomRIO, RandomGen (next), Random (randomR))
import Data.Char
extendedEuclidean :: Int -> Int -> (Int, Int, Int)
extendedEuclidean e m =
  if m == 0
  then (e, 1, 0)
  else (d, b, a - (e `div` m) * b)
  where
    (d, a, b) = extendedEuclidean m (e `mod` m)

eE2 e m | mod e m == 0 = (0, 1)
        | otherwise    = (snd abTupel  , fst abTupel   - (div e m) * snd abTupel)
        --Return (a,b) = (a = unteres b, b = unteres a -   e/m     *  unteres b )        
        -- Formeln (a,b):  1a = 2b , 1b = 2a - (1e/1m * 2b)
        where abTupel =   eE2 m        (mod e m)
        --nächstes e,m=       2e = m  2m = mod e m 


b = 2^2048
c = 12324
lean 0 = 0
lean a = if a > 9 then 1 + lean (div  a 10)
          else 1



bekommeZufallPrime :: (Random a, RandomGen g, Integral b, Num a) => g -> b -> (a, g)
bekommeZufallPrime seed  laenge= langeZufallszahl seed laenge
