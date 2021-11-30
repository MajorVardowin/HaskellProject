module Vigenere
(
    mainVigenere
)
where
import System.IO

-- Start der Vigenere-Chiffre
mainVigenere = do
    putStrLn "Möchtest du:"
    putStrLn "1. Verschlüsseln"
    putStrLn "2. Entschlüsseln"
    putStrLn "3. Beenden"
    putStrLn "Eingabe bitte mittels 1, 2 oder 3"
    putStr ">> "
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
                mainVigenere -- Rekursiver Aufruf, hält das Programm am Leben
    if decide == "3"
    then putStrLn "" -- Beendet das Programm
    else do 
        mainVigenere -- Rekursiver Aufruf, hält das Programm am Leben


verschl = do -- Verschlüsselung
    putStrLn "Wie möchtest du die Nachricht eingeben?"
    input <- inputMethode "Nachricht.txt" -- bestimmt, ob input aus "Nachricht.txt" oder von der Kommandozeile kommt
    putStrLn "Wie lautet der Schluessel?"
    putStr ">> "
    key <- getLine -- Key muss ein String sein

    writeFile "VingereKey.txt" key -- Key wird gespeichert

    let
        out = vigenereVer input key -- out ist verschlüsselte Nachricht

    -- Ausgabe und Speichern der verschlüsselten Nachricht
    writeFile "Verschl.txt" out
    putStrLn "\n\tDie verschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n")

entschl = do -- Entschlüsselung
    putStrLn "Woher stammt die Verschlüsselte Nachricht?"
    input <- inputMethode "Verschl.txt" -- bestimmt, ob input aus "Verschl.txt" oder von der Kommandozeile kommt
    
    putStrLn "Als letztes den Key!"
    key <- inputMethode "VingereKey.txt"  -- bestimmt, ob key aus "VingereKey.txt" oder von der Kommandozeile kommt

    let
        out = vigenereEnt input key

    -- Ausgabe und Speichern der entschlüsselten Nachricht
    writeFile "Nachricht.txt" out
    putStrLn "\n\tDie entschlüsselte Nachricht lautet: "
    putStrLn ("\t" ++ out ++ "\n")


inputMethode datei = do
    -- Liest Eingabe aus Datei oder von der Kommandozeile
    -- gibt die entsprechende Eingabe zurück
    putStrLn ("1. Aus Datei lesen [" ++ datei ++ "]")
    putStrLn "2. Über Kommandozeile eingeben (Standard)"
    putStr ">> "
    inputM <- getLine

    if inputM == "1"
    then do
        handle <- openFile datei ReadMode
        input <- hGetContents handle
        return input
    else do
        putStrLn "Bitte eingeben:"
        putStr ">> "
        getLine

-- Verschlüsselung
vigenereVer nachricht schluessel  =  vigenere nachricht schluessel "" 1

--Entschlüsselung
vigenereEnt nachricht schluessel  = vigenere nachricht schluessel "" (-1)

-- Itteriert für jeden Buchstaben durch
vigenere nachricht  [] rest richtung  = []
vigenere []  schluessel rest richtung = []
vigenere nachricht schluessel [] richtung= vigenere nachricht schluessel schluessel richtung
vigenere nachricht schluessel rest richtung= (toEnum(erechnetWert (head nachricht) (head rest) richtung)::Char) : vigenere (tail nachricht) schluessel (tail rest) richtung

-- Addiert den Buchstaben und den Buchstaben des Keys
erechnetWert buchst keyB richtung | großBuchstabe   = erzeugeAsciiBuchstabe (mod addiereBuchstG anzahlGroßer) groß
                            | kleinBuchstabe  = erzeugeAsciiBuchstabe (mod addiereBuchstK anzahlKleiner) klein
                            | otherwise = zahlA
    where
          großBuchstabe = zahlA >= 65 && zahlA <= 90  || zahlA == 196 || zahlA == 214 || zahlA == 220
          kleinBuchstabe = zahlA >= 97 && zahlA <= 122 || zahlA == 228 || zahlA == 246 || zahlA == 252 || zahlA == 223
          addiereBuchstG = berechnetKeyBuchstabenGroß + berechneAsciiZahl zahlA groß
          addiereBuchstK = berechnetKeyBuchstabenKlein + berechneAsciiZahl zahlA klein
          berechnetKeyBuchstabenGroß = anzahlGroßer + richtung*mod zahlB anzahlGroßer
          berechnetKeyBuchstabenKlein =  anzahlKleiner + richtung*mod zahlB anzahlKleiner -- z.B. b= 55, entschlüsseln (richtung -1), 30 Zeichen:   30-(25) = 5
          zahlA = fromEnum buchst :: Int
          zahlB = fromEnum keyB :: Int
          anzahlKleiner = 30
          anzahlGroßer = 29
          groß = 0
          klein = 1

-- Formt zur erleichterten Berechnung um
berechneAsciiZahl neuerWert  0 | neuerWert == 196 = 26
                             | neuerWert == 214 = 27
                             | neuerWert == 220 = 28
                             | otherwise = neuerWert -65

-- Formt zur erleichterten Berechnung um
berechneAsciiZahl neuerWert  1 | neuerWert == 228 = 26
                             | neuerWert == 246 = 27
                             | neuerWert == 252 = 28
                             | neuerWert == 223 = 29
                             | otherwise = neuerWert -97

-- Formt von der erleichterten Berechnung wieder zurück
erzeugeAsciiBuchstabe neuerWert  0 | mod neuerWert 29 == 26 = 196
                                    | mod neuerWert 29 == 27 = 214
                                    | mod neuerWert 29 == 28  = 220
                                    | otherwise = 65 + mod neuerWert 29

-- Formt von der erleichterten Berechnung wieder zurück
erzeugeAsciiBuchstabe neuerWert  1 | mod neuerWert 30 == 26 = 228
                                    | mod neuerWert 30 == 27 = 246
                                    | mod neuerWert 30 == 28 = 252
                                    | mod neuerWert 30 == 29 = 223
                                    | otherwise = 97 + mod neuerWert 30
