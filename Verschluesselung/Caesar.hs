module Caesar
(
    mainCaesar
)
where
import System.IO

caesar :: [Char] -> Int -> Int -> [Char]
caesar wort verschiebung richtung = caesarTextZusammen wort (richtung * verschiebung)


--caesarTextZusammen :: [Char] -> Int -> [Char]
caesarTextZusammen [] verschiebung = []
caesarTextZusammen wort verschiebung = (toEnum (berechneCaesar (head wort) verschiebung)::Char) : caesarTextZusammen (tail wort) verschiebung


--berechneCaesar :: Char -> Int -> Int
berechneCaesar a b  | zahlA >= 65 && zahlA <= 90 = mod (zahlA - 65 + b) laengeAlphabet + 65
                    | zahlA >= 97 && zahlA <= 122 = mod (zahlA - 97 + b) laengeAlphabet + 97
                    | otherwise = zahlA
    where zahlA = fromEnum a
          laengeAlphabet = 26

mainCaesar = do
    putStrLn "Möchtest du:"
    putStrLn "1. Verschlüsseln"
    putStrLn "2. Entschlüsseln"
    putStrLn "3. Beenden"
    putStrLn "Eingabe bitte mittels 1, oder 2"
    putStr ">>"
    decide <- getLine

    if decide == "1"
    then do
        verschl
    else 
        if decide == "2"
        then do
            entschl
        else
            if decide == "3"
            then 
                putStrLn "Ende"
            else do
                mainCaesar
    if decide == "3"
    then putStrLn "Ende"
    else do 
        mainCaesar


verschl = do
    putStrLn "Wie möchtest du die Nachricht eingeben?"
    input <- inputMethode "Nachricht.txt"
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine

    let
        out = caesar input (read key) 1
    writeFile "Verschl.txt" out
    putStrLn "\n\tDie verschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n")

entschl = do
    putStrLn "Woher stammt die Verschlüsselte Nachricht?"
    input <- inputMethode "Verschl.txt"
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine

    let
        out = caesar input (read key) (-1)
    writeFile "Nachricht.txt" out
    putStrLn "\n\tDie entschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n")


inputMethode datei = do
    putStrLn "Möchtest du:"
    putStrLn ("1. Aus Datei lesen [" ++ datei ++ "]")
    putStrLn "2. Über Kommandozeile eingeben (Standard)"
    putStr ">>"
    inputM <- getLine

    if inputM == "1"
    then do
        handle <- openFile datei ReadMode
        input <- hGetContents handle
        return input
    else do
        getLine
    