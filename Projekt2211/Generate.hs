module Generate
(
generateE, generateN, generateM, generateD, checkE
)where

generateE m = head [x | x<-isPrime m, mod m x /= 0]
--  generateE (generateM 7 11)
--generateE2 m =

generateN p q = p*q
-- Produkt der Primzahlen

generateM q p =   (q - 1) * (p - 1)
-- Produkt der verringerten Primzahlen

generateD e m = if d < 0 then m + d else d
        where d =fst (euklidAlgo e m)

euklidAlgo e m | mod e m == 0 = (0, 1)
        | otherwise    = (snd abTupel  , fst abTupel   - div e m * snd abTupel)
        --Return (a,b) = (a = unteres b, b = unteres a -   e/m     *  unteres b )        
        -- Formeln (a,b):  1a = 2b , 1b = 2a - (1e/1m * 2b)
        where abTupel =   euklidAlgo m        (mod e m)
        --nächstes e,m=       2e = m           2m = mod e m 

--if mod a 1000 > 255 then 
{-Funktion, die überprüft, ob e korrekt gewählt wurde-}
checkE e = last (isPrime e) == e


{-PRIMZAHLEN-}
isPrime n = myPrimeList [2..n]

myPrimeList ls  | null ls = []
                | otherwise = head ls : myPrimeList (hMyPrim (head ls) (tail ls))

hMyPrim n ts    | null ts = []
                | head ts `mod` n == 0 = hMyPrim n (tail ts)
                | otherwise = head ts : hMyPrim n (tail ts)

