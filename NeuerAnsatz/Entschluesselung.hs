module Entschluesselung
(
entschl
)where
import AsciiConverter 
import System.IO

entschl = do

    handle <- openFile "d.txt" ReadMode
    dTemp <- hGetContents handle

    handle <- openFile "n.txt" ReadMode
    nTemp <- hGetContents handle

    putStrLn "Übergangseingabe"
    putStr ">>"
    eingabe <- getLine

    let
        d = read dTemp
        n = read nTemp
        v = read eingabe
        -- Für jedes Element des Arrays anwenden
        vInString = asciiIntToString v -- Ab hier entschlüsseln
        vWiederInInt = asciiStringtoInt vInString
        vFormatiertAlsString = show vWiederInInt
        vEntschlüsselt = rsaSchluesseln  vFormatiertAlsString  d n
        entschluesseltAscii = asciiIntToString vEntschlüsselt


    putStrLn entschluesseltAscii



rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n