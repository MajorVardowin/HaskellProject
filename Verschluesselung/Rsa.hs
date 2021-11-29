module Rsa
(
mainRsa
)where
import Generate
import Verschluesselung
import Entschluesselung

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
        generateDo
    else 
        if decide == "2"
        then do
            verschlMain
        else
            if decide == "3"
            then 
                entschl
            else do
                if decide == "4"
                then 
                    putStrLn "Ende"
                else do 
                    mainRsa
    if decide == "4"
    then putStrLn "Ende"
    else do 
        mainRsa
    --putStrLn "Ende"