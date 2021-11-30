module Rsa
(
mainRsa
)where
import Generate
import Verschluesselung
import Entschluesselung

-- Startpunkt des RSA-Verschlüsselungsverfahren
mainRsa = do
    putStrLn "Möchtest du:"
    putStrLn "1. Primzahlengenerieren"
    putStrLn "2. Verschlüsseln"
    putStrLn "3. Entschlüsseln"
    putStrLn "4. Beenden"
    putStrLn "Eingabe bitte mittels 1, 2, 3 oder 4"
    putStr ">>"

    decide <- getLine

    if decide == "1"
    then do
        generateDo -- Primzahlen generieren
    else 
        if decide == "2"
        then do
            verschlMain -- Verschlüsseln
        else
            if decide == "3"
            then 
                entschl -- Entschlüsseln
            else do
                if decide == "4"
                then 
                    putStrLn "Ende" -- Beenden
                else do 
                    mainRsa -- Rekursiver Aufruf, damit Programm am Leben bleibt
    if decide == "4"
    then putStrLn "Ende" -- Beenden
    else do 
        mainRsa -- Rekursiver Aufruf, damit Programm am Leben bleibt
    --putStrLn "Ende"