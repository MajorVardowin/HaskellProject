module ProjektB
(
main
)where

import System.IO
import System.Random (randomRIO, Random (randomR, randomRs), RandomGen (next), newStdGen, getStdRandom, StdGen)
import Generate;
import AsciiConverter
import Data.Char



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

{-
generierePrimBereichTest leng ersteZahl  = do
        zahl <- langeZufallszahl leng
        if odd zahl && checkE  zahl  then return zahl else generierePrimBereichTest leng ersteZahl -- EIGENTLICH PRIMABSTAND abfragen

langeZufallszahlTest :: Integer -> IO Integer
langeZufallszahlTest leng = randomRIO (leng, leng)
-}


main  = do

        putStrLn "Möchtest du aus der Datei einlesen?"
        putStrLn "Dann bitte [einlesen] eingeben!"
        putStrLn "Ansonsten erfolgt die Eingabe über die Konsole."
        putStr ">>"
        einlesen <- getLine

        if map toLower einlesen == "einlesen"
        then readInput
        else consoleInput


readInput = do
        handle <- openFile "Input.txt" ReadMode
        contents <- hGetContents handle

        if not (null contents)
        then do putStrLn contents
                start contents
        else do putStrLn "Read null"
                putStrLn "End"
        hClose handle

consoleInput = do
        putStrLn "String für das Ver-/Entschlüsseln eingeben bitte:"
        putStr ">>"
        eingabe <- getLine

        if not (null eingabe)
        then do start eingabe
        else do putStrLn "Read null"
                putStrLn "End"



entschlüsseln eingabe = do
        putStrLn "End"

start eingabe = do

        putStrLn "Wenn du entschlüsseln möchtest, bitte [entschlüsseln] eingeben!"
        putStrLn "Ansonsten folgt das Verschlüsseln."
        putStr ">>"
        art <- getLine

        if map toLower art == "entschlüsseln"
        then entschlüsseln eingabe

        else do

        let len = 10
        p <- generierePrimBereich len 0
        q <- generierePrimBereich len p

        let
                m = generateM p q
                n = generateN p q
                e = generateE m
                d = generateD e m
                i = 1+1 -- wie viele Buchstaben immer zusammengepackt werden --> TEST = [TE, ST]
                -- Ich wollte das hier festlegen und dann unten das bei >arr = test eingabe i< einbinden.
                -- Problem ist, dann hat i den Typ any und die Funktionen unten klappen nicht mehr -.-

        let
                t = asciiStringtoInt eingabe
                tFormatiert = show t

                v = rsaSchluesseln tFormatiert e n -- v == verschlüsselte Nachricht


                arr = eingabeInArray eingabe
                arr2 = verschlVonArr arr e n
        
        
        -- Den Teil wollte ich in entschlüsseln packen, dann kommt nur noch Mist raus -.-
                vInString = asciiIntToString v -- Ab hier entschlüsseln
                vWiederInInt = asciiStringtoInt vInString
                vFormatiertAlsString = show vWiederInInt
                vEntschlüsselt = rsaSchluesseln  vFormatiertAlsString  d n
                entschluesseltAscii = asciiIntToString vEntschlüsselt
        -- Den Teil wollte ich in entschlüsseln packen, dann kommt nur noch Mist raus -.-
        
        putStrLn "arr = "
        print arr
        putStrLn "arr2 = "
        print arr2

        putStrLn ("eingabe = " ++ eingabe)
        putStrLn ("t = " ++ show t)
        putStrLn ("p = " ++ show p ++ "; q = " ++ show q)
        putStrLn ("m = " ++ show m ++ "; n = " ++ show n)
        putStrLn ("e = " ++ show e ++ "; d = " ++ show d)
        putStr ("v = " ++ show v ++ "; als String = ")
        print vInString
        putStrLn "---"
        putStrLn "---"
        putStrLn ("v in Int = " ++ show vWiederInInt)
        putStrLn ("v Entschlüsselt = " ++ show vEntschlüsselt)
        putStrLn ("Entschlüsseltes ASCII = " ++ entschluesseltAscii)


goWrite string = do
    writeFile "Output.txt" string
    putStrLn "File beschrieben"

rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n

eingabeInArray []  = []
eingabeInArray str = asciiStringtoInt (take 2 str) : eingabeInArray (drop 2 str)

verschlVonArr rr e n = map (\ a -> rsaSchluesseln (show a) e n) rr 
{-
-- Das wurde mir vorgeschlagen, nachdem ich ne manuelle Map-Funktion geschrieben hatte
        verschlVonArr [] e n = []
        verschlVonArr (a:rr) e n = rsaSchluesseln a e n : verschlVonArr rr e n
-}






