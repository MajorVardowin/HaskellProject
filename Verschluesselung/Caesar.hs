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
    input <- inputMethode "Nachricht.txt"
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine

    let
        out = caesar input (read key) 1
    writeFile "Verschl.txt" out
    putStrLn "Die verschlüsselte Nachricht lautet: "
    putStrLn out

entschl = do
    input <- inputMethode "Verschl.txt"
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine

    let
        out = caesar input (read key) (-1)
    writeFile "Nachricht.txt" out
    putStrLn "Die entschlüsselte Nachricht lautet: "
    putStrLn out


inputMethode datei = do
    putStrLn "Möchtest du:"
    putStrLn "1. Aus Datei lesen"
    putStrLn "2. Über Kommandozeile eingeben"
    putStr ">>"
    inputM <- getLine

    if inputM == "1"
    then do
        handle <- openFile datei ReadMode
        input <- hGetContents handle
        return input
    else do
        getLine
    