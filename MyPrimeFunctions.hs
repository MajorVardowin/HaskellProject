module MyPrimeFunctions
(
  check
) where

{-PRIMZAHLEN-}
isPrime n = myPrimeList [2..n]

myPrimeList ls  | null ls = []
                | otherwise = head ls : myPrimeList (hMyPrim (head ls) (tail ls))

hMyPrim n ts    | null ts = []
                | head ts `mod` n == 0 = hMyPrim n (tail ts)
                | otherwise = head ts : hMyPrim n (tail ts)

{-PRIMFAKTORZERLEGUNG-}
primeFactors 1 = []
primeFactors n  | null factors  = [n]
                | otherwise = factors ++ primeFactors (n `div` head factors)
  where factors = take 1 $ filter (\x -> n `mod` x == 0) [2 .. n-1]

{-Funktion, die überprüft, ob e korrekt gewählt wurde-}
check m e = (last (isPrime e) == e) && notElem e (primeFactors (m `div` 2)) -- ist e Primzahl und e nicht in Primfaktorzerlegung von m/2