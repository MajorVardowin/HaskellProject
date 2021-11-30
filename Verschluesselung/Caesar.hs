module Caesar
(
    mainCaesar
)
where
import System.IO

-- Generelle Aufruf, entscheidet ob ent-, oder verschlüsselt wird, durch Parameter 'richtung' 
caesar :: [Char] -> Int -> Int -> [Char]
caesar wort verschiebung richtung = caesarTextZusammen wort (richtung * verschiebung)


-- Verschlüsselung, die jeden Buchstaben einzeln verschiebt
-- Dafür wird der ASCII-Wert um den Wert der Verschiebung angepasst
caesarTextZusammen [] verschiebung = []
caesarTextZusammen wort verschiebung = (toEnum (berechneCaesar (head wort) verschiebung)::Char) : caesarTextZusammen (tail wort) verschiebung


-- a ist Buchstabe b ist die Verschiebung
-- liefert verschobenes a zurück
berechneCaesar a b  | zahlA >= 65 && zahlA <= 90 = mod (zahlA - 65 + b) laengeAlphabet + 65
                    | zahlA >= 97 && zahlA <= 122 = mod (zahlA - 97 + b) laengeAlphabet + 97
                    | otherwise = zahlA
    where zahlA = fromEnum a
          laengeAlphabet = 26

-- Start der Caesar-Chiffre
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
        verschl -- startet die Verschlüsselung
    else 
        if decide == "2"
        then do
            entschl -- startet die Entschlüsselung
        else
            if decide == "3"
            then 
                putStrLn "Ende" -- Beendet das Programm
            else do
                mainCaesar -- Rekursiver Aufruf, hält Programm am "Leben"
    if decide == "3"
    then putStrLn "" -- Beendet das Programm
    else do 
        mainCaesar -- Rekursiver Aufruf, hält Programm am "Leben"


verschl = do
    putStrLn "Wie möchtest du die Nachricht eingeben?"
    input <- inputMethode "Nachricht.txt" -- bestimmt, ob input aus "Nachricht.txt" oder von Kommandozeile kommt
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine -- key ist die Verschiebung

    let
        out = caesar input (read key) 1 -- verschlüsselt die Eingabe -> Richtung ist = 1 
    
    -- Ausgabe und Speichern der verschlüsselten Nachricht
    writeFile "Verschl.txt" out
    putStrLn "\n\tDie verschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n") 

entschl = do
    putStrLn "Woher stammt die Verschlüsselte Nachricht?"
    input <- inputMethode "Verschl.txt" -- bestimmt, ob input aus "Verschl.txt" oder von Kommandozeile kommt
    putStrLn "Wie lautet der Schlüssel?"
    putStr ">>"
    key <- getLine

    let
        out = caesar input (read key) (-1) -- entschlüsselt die Eingabe -> Richtung ist = -1 
    
    -- Ausgabe und Speichern der verschlüsselten Nachricht
    writeFile "Nachricht.txt" out
    putStrLn "\n\tDie entschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n")


inputMethode datei = do
    -- Liest Eingabe aus Datei oder von der Kommandozeile
    -- gibt die entsprechende Eingabe zurück
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
    