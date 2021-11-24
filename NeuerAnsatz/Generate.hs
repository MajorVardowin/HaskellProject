module Generate
(
generateE, generateN, generateM, generateD, checkE, generierePrimBereich, generateDo
)where

import System.Random (randomRIO, Random (randomR, randomRs), RandomGen (next), newStdGen, getStdRandom, StdGen)

generateDo = do
    let len = 10
    p <- generierePrimBereich len 0
    q <- generierePrimBereich len p
    let
        m = generateM p q
        n = generateN p q
        e = generateE m
        d = generateD e m
    writeFile "d.txt" (show d)
    writeFile "e.txt" (show e)
    writeFile "n.txt" (show n)
    print ("Oeffentliche Schluessel: e = ", e, " n = ", n)


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


generierePrimBereich leng ersteZahl  = do
        if leng < 4 then generierePrimBereich 4 ersteZahl
        else do
        zahl <- langeZufallszahl leng
        if odd zahl && checkE  zahl && ersteZahl /= zahl then return zahl else generierePrimBereich leng ersteZahl -- EIGENTLICH PRIMABSTAND abfragen

--langeZufallszahl2 :: (Random a, RandomGen g, Num a) => g -> (a, g)
langeZufallszahl :: Integer -> IO Integer
langeZufallszahl laenge         | laenge < 2 =  randomRIO (0,1)-- seed
                                | otherwise = randomRIO (2^(laenge-1),2^laenge-1)-- seed

t laenge = randomRIO (2^(laenge-1),2^laenge-1)

primAbstand a b | logAbstand < 0 = primAbstand b a
                | logAbstand < 0.1 || logAbstand > 30 = False
                |  otherwise = True
                        where logAbstand = logBase 2 a - logBase 2 b

