module Entschluesselung
(
entschl
)where
import AsciiConverter
import System.IO
import StringToIntArr

-- Startpunkt der RSA-Entschlüsselung
entschl = do

    handle <- openFile "d.txt" ReadMode -- private Schlüssel laden
    dTemp <- hGetContents handle

    handle <- openFile "n.txt" ReadMode -- öffentlichen Schlüssel laden
    nTemp <- hGetContents handle

    handle <- openFile "Verschl.txt" ReadMode -- verschlüsselte Nachricht laden
    inputTemp <- hGetContents handle

    let
        input = map toInteger (nimm inputTemp) -- String "[123,456,789]" in ein richtiges Integer Array umformen
        d = read dTemp
        n = read nTemp

        -- Schritt für Schritt Entschlüsselung
        result = map asciiIntToString input -- jeden Int-Wert in einen String umformen
        result2 = map asciiStringtoInt result -- berechnen in ein int Array
        result3 = map show result2 -- die int-Werte in String umformen
        result4 = map (\ a -> rsaSchluesseln a d n) result3 -- RSA-Verschlüsselung mit a = Elemente der Nachricht, d = privater Schlüssel und n = öffentlicher Schlüssel
        result5 = map asciiIntToString result4 -- Die Integer-Werte in Buchstabenpaare umformen
        endresult = concat result5 -- Buchstabenpaare in einen String konkatenieren
        
    writeFile "Nachricht.txt" endresult -- entschlüsselte Nachricht speichern
    
    -- Schritt für Schritt Ausgabe
    print result
    print result2
    print result3
    print result4
    print result5

    -- Ausgabe der Verschlüsselten Nachricht
    putStrLn ""
    putStrLn "\n\tDie entschlüsselte Nachricht lautet: "
    putStr ("\t" ++ endresult)
    putStrLn "\n"
    
    hClose handle


-- RSA Entschlüsselung
rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n 