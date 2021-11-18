{-# OPTIONS_GHC -Wno-deprecations #-}
--{-# LANGUAGE FlexibleContexts #-}
module RateZahl
    where

import System.IO (getLine)
import System.Random (randomRIO, Random (randomR, randomRs), RandomGen (next), newStdGen, getStdRandom, StdGen)



generierePrim :: Integer  -> IO Integer
generierePrim leng  = do

        zahl <- langeZufallszahl leng
        seed <- newStdGen
        if odd zahl && isPrime seed zahl then return zahl else generierePrim leng

--langeZufallszahl2 :: (Random a, RandomGen g, Num a) => g -> (a, g)
langeZufallszahl :: Integer -> IO Integer
langeZufallszahl laenge     | laenge < 2 =  randomRIO (0,1)-- seed
                             | otherwise = randomRIO (2^(laenge-1),2^laenge-1)-- seed


main len = do
        p <- generierePrim len
        q <- generierePrim len
        print (25000 ^ q)
        --print (schluessel ( schluessel 2  (generateE (erhalteM p q))   p*q) (generateD (generateE (erhalteM p q)) (erhalteM p q) ) p*q )
        -- geht nicht dauert so zu lange
        --print(p,q,erhalteM p q, generateE (erhalteM p q) ,generateD (generateE (erhalteM p q)) (erhalteM p q))
erhalteM q p =   (q - 1) * (p - 1)

generateE m = head [x | x<-isPrimeJan m, mod m x /= 0]
--  generateE (erhalteM 7 11)

{-PRIMZAHLEN-}
isPrimeJan n = myPrimeList [2..n]

myPrimeList ls  | null ls = []
                | otherwise = head ls : myPrimeList (hMyPrim (head ls) (tail ls))

hMyPrim n ts    | null ts = []
                | head ts `mod` n == 0 = hMyPrim n (tail ts)
                | otherwise = head ts : hMyPrim n (tail ts)


generateD e m = if d < 0 then m + d else d
        where d =fst (euklidAlgo e m)
schluessel textZahlen ed n = mod (textZahlen^ed) n



euklidAlgo e m | mod e m == 0 = (0, 1)
        | otherwise    = (snd abTupel  , fst abTupel   - (div e m) * snd abTupel)
        --Return (a,b) = (a = unteres b, b = unteres a -   e/m     *  unteres b )        
        -- Formeln (a,b):  1a = 2b , 1b = 2a - (1e/1m * 2b)
        where abTupel =   euklidAlgo m        (mod e m)
        --nächstes e,m=       2e = m  2m = mod e m 














--------------------------------------------------------------------------------
-- generates a t-bounded random list of n elements
randomList :: (RandomGen g, Random a) => g -> (a,a) -> Int -> [a]
randomList s t n = take n $ randomRs t s
--------------------------------------------------------------------------------
-- binary exponenciation algorithm :: a^e mod n
power :: (Integral a) => a -> a -> a -> a
power a 1 n = a
power a e n = (a^(e `mod` 2) * power (a*a `mod` n) (e `div` 2) n) `mod` n
--------------------------------------------------------------------------------
-- Given x returns (y,z) where x = 2^y * z and z odd
twoPowersFact :: (Integral a) => a -> (a,a)
twoPowersFact 0 = (0,0)
twoPowersFact m = let go (s,n)
                         | even n    = go (s+1,n `div` 2)
                         | otherwise = (s,n)
                  in go (0,abs m)
--------------------------------------------------------------------------------
-- PseudoPrimality Test :: Miller Rabin
-- millerRabin possiblePrime testBase
millerRabin :: (Integral a) => a -> a ->  Bool
millerRabin n a = let fact = twoPowersFact (n-1);
                      base = power a (snd fact) n;
                      test b s r n
                       | r == s = b == n-1
                       | b == 1 && r /= 0 = False
                       | otherwise = b == n-1 || test (power b 2 n) s (r+1) n
                  in base == 1 || test base (fst fact) 0 n
--------------------------------------------------------------------------------
-- isPrimePure possiblePrime [list of testBases]
isPrimePure :: (Eq a, Integral a) => a -> [a] -> Bool
isPrimePure 1 _  = False
isPrimePure 2 _  = True
isPrimePure _ [] = False -- should not be true or false 
isPrimePure can wit = let primes = [3,5,7,11,13,17,19,23,29,31] in
                   all (\ p -> p >= can || (can `mod` p) /= 0) primes
                && foldr ((&&) . millerRabin can) True wit
--------------------------------------------------------------------------------
-- the longer is the randomList, the higher is the accuracy of the test
isPrime :: (RandomGen g, Integral a, Random a) => g -> a -> Bool
isPrime seed can = isPrimePure can $ randomList seed (2,can-2) 20
--------------------------------------------------------------------------------






{-
-- the next prime after num
nextPrime :: (RandomGen g, Integral a, Random a) => g -> a -> a
nextPrime seed num
    | even num         = nextPrime (snd $ next seed) (num+1)
    | isPrime seed num = num
    | otherwise        = nextPrime (snd $ next seed) (num+2)
--------------------------------------------------------------------------------


nBitsRanNum  laenge = if laenge <10 then nBitsRanNum 10 else  randomRIO (2^(laenge-1),2^laenge-1)
--------------------------------------------------------------------------------
-- random prime of nbits

randomPrime  nbits = nBitsRanNum nbits
--------------------------------------------------------------------------------


-}









{-main3 = do
        a <- main2 3
        if a /= 11
        then return a
        else main3
testing :: Integer -> Integer
testing a = mod a 7-}





{-rate zahl anzVersuche = do
  putStrLn "Gib deinen Wert ein: "
  -- Liest einen String ein
  rateZ <- getLine
  -- macht einen Int-Typ daraus
  let wert = read rateZ
  if wert < zahl
    then do putStrLn "Deine Zahl ist zu niedrig"
            rate zahl (anzVersuche+1)
    else if wert > zahl
           then do putStrLn "Deine Zahl ist zu hoch"
                   rate zahl (anzVersuche+1)
           else do putStr "Richtig! Du hast "
                   putStr (show (anzVersuche))
                   putStrLn " Versuche gebraucht."-}
{---main :: (Integral a, Random a) => a -> IO()
main :: Integer -> Integer -> IO Integer
main leng a = do

        zahl <- langeZufallszahl leng
        seed <- newStdGen
        if a > 0
        then    if isPrime seed zahl
                then return ((zahl - 1) * (a - 1))
                else main leng a
        else    if isPrime seed zahl
                then main leng zahl
                else main leng 0
-}