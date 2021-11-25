import Generate
import Verschluesselung
import Entschluesselung

main = do
    putStrLn "Möchtest du:"
    putStrLn "1. Primzahlengenerieren"
    putStrLn "2. Verschlüsseln"
    putStrLn "3. Entschlüsseln"
    putStrLn "Eingabe bitte mittels 1, 2, oder 3"
    putStr ">>"

    decide <- getLine

    if decide == "1"
    then generateDo
    else    if decide == "2"
            then do verschlMain
            else do entschl

    putStrLn "Ende"