module Generate
(
generateE, generateN, generateM, generateD, checkE, generierePrimBereich, generateDo
)where

import System.Random (randomRIO, Random (randomR, randomRs), RandomGen (next), newStdGen, getStdRandom, StdGen)

-- generiert Parameter für RSA Verschlüsselung
-- diese werden in Extradateien gespeichert
-- Öffentliche Schlüssel werden ausgegeben
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
    putStrLn ("\n\tÖffentliche Schluessel: e = " ++ show e ++ " n = " ++ show n)
    putStr "\n"


-- generiert kleinstes e, e muss Primzahl sein und darf kein Teiler von m sein
generateE m = head [x | x<-isPrime m, mod m x /= 0]


-- n ist das Produkt der Primzahlen
generateN p q = p*q


-- m ist das Produkt der verringerten Primzahlen
generateM q p =   (q - 1) * (p - 1)


-- d ist das Ergebnis des eukl. Algorithmus
-- d muss positiv sein
generateD e m = if d < 0 then m + d else d
        where d = fst (euklidAlgo e m)

-- Umsetzung des eukl. Algorithmus
euklidAlgo e m | mod e m == 0 = (0, 1)
        | otherwise    = (snd abTupel  , fst abTupel   - div e m * snd abTupel)
        --Return (a,b) = (a = unteres b, b = unteres a -   e/m   *  unteres b )        
        -- Formeln (a,b):  1a = 2b , 1b = 2a - (1e/1m * 2b)
        where abTupel =   euklidAlgo m        (mod e m)
        --nächstes e,m=       2e = m           2m = mod e m 


--Funktion, die überprüft, ob e korrekt gewählt wurde (ist e eine Primzahl)
checkE e = last (isPrime e) == e


{-PRIMZAHLEN-}
-- generiert eine Liste von Primzahlen 2 bis n
isPrime n = myPrimeList [2..n]

-- Alle vielfachen des ersten Elements aus ls werden gelöscht
-- Angefangen bei n = 2 werden alle größeren Zahlen in ls überprüft
-- gibt Liste von Primzahlen 2 bis n wieder
myPrimeList ls  | null ls = []
                | otherwise = head ls : myPrimeList (hMyPrim (head ls) (tail ls))

-- Alle Elemente aus ts werden modulo n gerechnet. 
-- Wenn das = 0 ist, ist das Element aus ts ein Vielfaches von n -> keine Primzahl
-- Gibt eine Liste von Zahlen wieder, die > n und kein Vielfaches von n sind
hMyPrim n ts    | null ts = []
                | head ts `mod` n == 0 = hMyPrim n (tail ts)
                | otherwise = head ts : hMyPrim n (tail ts)

-- Überprüfen ob Primzahl groß genug ist

generierePrimBereich leng ersteZahl  = do
        if leng < 4 then generierePrimBereich 4 ersteZahl
        else do
        zahl <- langeZufallszahl leng -- generiert zufällige Zahl
        if odd zahl && checkE  zahl && ersteZahl /= zahl then return zahl else generierePrimBereich leng ersteZahl 
        -- überprüft, ob Zahl ungerade, Primzahl und ungleich der anderen Primzahl ist
        -- im ersten Aufruf wird einfach nur eine Primzahl generiert (Zeile 13)
        -- im zweiten Aufruf wird die erste Primzahl mitgeliefert und auf Ungleichheit getestet (Zeile 14)

-- Zufallszahl wird generiert
langeZufallszahl :: Integer -> IO Integer
langeZufallszahl laenge         | laenge < 2 =  randomRIO (0,1)
                                | otherwise = randomRIO (2^(laenge-1),2^laenge-1)


-- Bei größeren Primzahlen muss der Abstand überprüft werden
primAbstand a b | logAbstand < 0 = primAbstand b a
                | logAbstand < 0.1 || logAbstand > 30 = False
                |  otherwise = True
                        where logAbstand = logBase 2 a - logBase 2 b

