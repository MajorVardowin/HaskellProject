module Main
(
    main
)
where
import Rsa
import Caesar
import Vingere


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
        mainRsa
    else 
        if decide == "2"
        then do
            mainVingere
        else 
            if decide == "3"
            then do mainCaesar
            else 
                if decide == "4"
                then putStrLn "Ende" 
                else do 
                    main
    if decide == "4"
    then 
        putStr "" 
    else do 
        main
    