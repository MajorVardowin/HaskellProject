module Verschluesselung
(
verschlMain
)where

import AsciiConverter
import System.IO


verschlMain = do
    putStrLn "Was möchtest du verschlüsseln?"
    putStrLn "1: Konsoleneingabe (Standard)"
    putStrLn "2: Aus Datei [Nachricht.txt]"
    putStr ">>"
    decision <- getLine
    
    if decision == "2"
    then do directory
    else do console

console = do
    putStrLn "Bitte den zu verschlüsselnden Wert eingeben"
    putStr ">>"
    eingabe <- getLine
    verschl eingabe

directory = do
    handle <- openFile "Nachricht.txt" ReadMode
    eingabe <- hGetContents handle
    verschl eingabe
    hClose handle

verschl eingabe = do
    
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

    writeFile "Verschl.txt" (show arr2)

    putStrLn "\n\tDie Verschlüsselte Nachricht lautet: "
    putStr  ("\t" ++ show arr2)
    putStrLn "\n"

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