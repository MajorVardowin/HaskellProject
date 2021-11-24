module Verschluesselung
(
verschl
)where

import AsciiConverter
import System.IO


verschl = do
    putStrLn "Bitte den zu verschlüsselnden Wert eingeben"
    putStr ">>"
    eingabe <- getLine

    handle <- openFile "e.txt" ReadMode
    eTemp <- hGetContents handle

    handle <- openFile "n.txt" ReadMode
    nTemp <- hGetContents handle

    let
        e = read eTemp
        n = read nTemp
        t = asciiStringtoInt eingabe
        tFormatiert = show t

        v = rsaSchluesseln tFormatiert e n -- v == verschlüsselte Nachricht

        arr = eingabeInArray eingabe
        arr2 = verschlVonArr arr e n

    writeFile "Output.txt" (show arr2)

    print arr2

    hClose handle



rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n

-- Eingabe so umformen, dass jeweils 2 Elemente als eine Zahl interpretiert werden
eingabeInArray []  = []
eingabeInArray str = asciiStringtoInt (take 2 str) : eingabeInArray (drop 2 str)

-- Verschlüsseln der Arraywerte
verschlVonArr rr e n = map (\ a -> rsaSchluesseln (show a) e n) rr 
{-
-- Das wurde mir vorgeschlagen, nachdem ich ne manuelle Map-Funktion geschrieben hatte
        verschlVonArr [] e n = []
        verschlVonArr (a:rr) e n = rsaSchluesseln a e n : verschlVonArr rr e n
-}