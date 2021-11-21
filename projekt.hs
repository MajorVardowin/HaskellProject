module RateZahl
    where

import System.IO (getLine)
import System.Random (randomRIO, Random (randomR, randomRs), RandomGen (next), newStdGen, getStdRandom, StdGen)




generierePrimBereich leng ersteZahl  = do
        if leng < 4 then generierePrimBereich 4 ersteZahl 
        else do
        zahl <- langeZufallszahl leng
        if odd zahl && checkJan zahl && ersteZahl /= zahl then return zahl else generierePrimBereich leng ersteZahl -- EIGENTLICH PRIMABSTAND abfragen



primAbstand a b | logAbstand < 0 = primAbstand b a
                | logAbstand < 0.1 || logAbstand > 30 = False 
                |  otherwise = True 
                        where logAbstand = logBase 2 a - logBase 2 b
                        
                                 

--langeZufallszahl2 :: (Random a, RandomGen g, Num a) => g -> (a, g)
langeZufallszahl :: Integer -> IO Integer
langeZufallszahl laenge     | laenge < 2 =  randomRIO (0,1)-- seed
                             | otherwise = randomRIO (2^(laenge-1),2^laenge-1)-- seed


main len = do

        putStrLn "Input: q"
        eingabe <- getLine
        p <- generierePrimBereich len 0
        q <- generierePrimBereich len p

        let
                t = woerterAneinanderKlatschen eingabe
                m = erhalteM p q
                n = generateN p q
                e = generateE m
                d = generateD e m
                v = schluessel t e n
                --v4 = toInteger v
                --vS = wandelInZeichen v4
                --vZ = woerterAneinanderKlatschen v
                tZurueck = schluessel (show v) d n

        print (eingabe, t,p,q,e,m,n,d, v, tZurueck)
erhalteM q p =   (q - 1) * (p - 1)


laengeAnpassen zahl =  if zahl < 100 then ('0':show zahl) else show(zahl)

umwandelnInBuchstabe zahl = toEnum zahl

umwandelnInZahl ch = fromEnum ch


woerterAneinanderKlatschen [] = ""
woerterAneinanderKlatschen (satz) =  laengeAnpassen (umwandelnInZahl ( head satz)) ++ (woerterAneinanderKlatschen (tail satz))



wandelInZeichen zahl  | zahl <= 0 = do ""
        | mod zahl 1000 <= 255 =  wandelInZeichen (div intZahl 1000) ++ [toEnum(mod intZahl 1000)]  
        | otherwise = wandelInZeichen (div intZahl  100) ++ [toEnum(mod intZahl 100)]  
                 where  intZahl :: (Integral a) => a
                        intZahl = fromInteger zahl



--if mod a 1000 > 255 then 
{-Funktion, die überprüft, ob e korrekt gewählt wurde-}
checkJan  e = (last (isPrimeJan e) == e)

generateE m = head [x | x<-isPrimeJan m, mod m x /= 0]
--  generateE (erhalteM 7 11)

generateN p q = p*q

{-PRIMZAHLEN-}
isPrimeJan n = myPrimeList [2..n]

myPrimeList ls  | null ls = []
                | otherwise = head ls : myPrimeList (hMyPrim (head ls) (tail ls))

hMyPrim n ts    | null ts = []
                | head ts `mod` n == 0 = hMyPrim n (tail ts)
                | otherwise = head ts : hMyPrim n (tail ts)


generateD e m = if d < 0 then m + d else d
        where d =fst (euklidAlgo e m)
schluessel textZahlen ed n = mod (read(textZahlen)^ed) n



euklidAlgo e m | mod e m == 0 = (0, 1)
        | otherwise    = (snd abTupel  , fst abTupel   - (div e m) * snd abTupel)
        --Return (a,b) = (a = unteres b, b = unteres a -   e/m     *  unteres b )        
        -- Formeln (a,b):  1a = 2b , 1b = 2a - (1e/1m * 2b)
        where abTupel =   euklidAlgo m        (mod e m)
        --nächstes e,m=       2e = m           2m = mod e m 









