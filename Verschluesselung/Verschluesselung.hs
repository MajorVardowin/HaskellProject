module Verschluesselung
(
verschlMain
)where

import AsciiConverter
import System.IO

-- Startpunkt der RSA-Verschlüsselung
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
    putStrLn "Bitte den zu verschlüsselnden Wert eingeben" -- Nachricht über die Konsole eingeben
    putStr ">>"
    eingabe <- getLine
    verschl eingabe

directory = do
    handle <- openFile "Nachricht.txt" ReadMode -- Nachricht aus Textdatei einlesen
    eingabe <- hGetContents handle
    verschl eingabe
    hClose handle

verschl eingabe = do
    
    handle <- openFile "e.txt" ReadMode -- lesen der öffentlichen Schlüssel
    eTemp <- hGetContents handle

    handle <- openFile "n.txt" ReadMode -- lesen der öffentlichen Schlüssel
    nTemp <- hGetContents handle

    let
        e = read eTemp
        n = read nTemp
        
        t = asciiStringtoInt eingabe
        tFormatiert = show t
        v = rsaSchluesseln tFormatiert e n -- v == verschlüsselte Nachricht

        arr = eingabeInArray eingabe -- Array mit umgeformter Nachricht
        arr2 = verschlVonArr arr e n -- Array mit verschlüsselter Nachricht

    writeFile "Verschl.txt" (show arr2) -- Verschlüsselte Nachricht in Textdatei speichern

    putStrLn "\n\tDie Verschlüsselte Nachricht lautet: " -- Verschlüsselte Nachricht über die Konsole ausgeben
    putStr  ("\t" ++ show arr2)
    putStrLn "\n"

    hClose handle


-- RSA Verschlüsselung
rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n

-- Eingabe so umformen, dass jeweils 2 Elemente als eine Zahl interpretiert werden
eingabeInArray []  = []
eingabeInArray str = asciiStringtoInt (take 2 str) : eingabeInArray (drop 2 str)

-- Verschlüsseln der Arraywerte
-- RSA-Verschlüsselung mit rr = Elemente der Nachricht, e = öffentlicher Schlüssel und n = öffentlicher Schlüssel
verschlVonArr rr e n = map (\ a -> rsaSchluesseln (show a) e n) rr 
{-
-- Übersichtlichere Schreibweise
        verschlVonArr [] e n = []
        verschlVonArr (a:rr) e n = rsaSchluesseln a e n : verschlVonArr rr e n
-}