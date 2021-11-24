module Entschluesselung
(
entschl
)where
import AsciiConverter 
import System.IO
import StringToIntArr

entschl = do

    handle <- openFile "d.txt" ReadMode
    dTemp <- hGetContents handle

    handle <- openFile "n.txt" ReadMode
    nTemp <- hGetContents handle

    handle <- openFile "Input.txt" ReadMode
    inputTemp <- hGetContents handle

    

    putStrLn "Übergangseingabe"
    putStr ">>"
    eingabe <- getLine

    let
        input = map toInteger (nimm inputTemp)
        d = read dTemp
        n = read nTemp
        v = head input
        -- Für jedes Element des Arrays anwenden
        vInString = asciiIntToString v -- Ab hier entschlüsseln
        vWiederInInt = asciiStringtoInt vInString
        vFormatiertAlsString = show vWiederInInt
        vEntschlüsselt = rsaSchluesseln  vFormatiertAlsString  d n
        entschluesseltAscii = asciiIntToString vEntschlüsselt

    putStrLn entschluesseltAscii
    hClose handle



rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n