module Main
(
    main
)
where
import Rsa
import Caesar
import Vigenere


-- "Hauptmenü" unseres Programms
main = do
    putStrLn "Hallo welche Verschlüsselung möchtest du Nutzen?"
    putStrLn "1. RSA"
    putStrLn "2. Vigenere"
    putStrLn "3. Caesar"
    putStrLn "4. Beenden"
    putStrLn "Eingabe bitte mittels 1, 2, 3, oder 4"
    putStr ">> "
    decide <- getLine

    if decide == "1"
    then do
        mainRsa -- RSA starten
    else 
        if decide == "2"
        then do
            mainVigenere -- Vigenere-Chiffre starten
        else 
            if decide == "3"
            then do mainCaesar -- Caesar-Chiffre starten
            else 
                if decide == "4"
                then putStrLn "Ende" -- Beenden des Programms
                else do 
                    main -- Rekursiver Aufruf um Programm am Leben zu halten
    if decide == "4"
    then 
        putStr "" -- Beenden
    else do 
        main -- Rekursiver Aufruf um Programm am Leben zu halten
    