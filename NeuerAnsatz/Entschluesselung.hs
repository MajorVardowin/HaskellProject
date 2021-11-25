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

    handle <- openFile "Verschl.txt" ReadMode
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
        vWiederInInt = show (asciiStringtoInt vInString)
        --vFormatiertAlsString = vWiederInInt
        vEntschlüsselt = rsaSchluesseln  vWiederInInt d n
        entschluesseltAscii = asciiIntToString vEntschlüsselt
        

        result = map asciiIntToString input
        result2 = map asciiStringtoInt result
        result3 = map show result2
        result4 = map (\ a -> rsaSchluesseln a d n) result3 
        result5 = map asciiIntToString result4
        endresult = concat result5
        {-
        verschlVonArr rr e n = map (\ a -> rsaSchluesseln (show a) e n) rr
        -}
    writeFile "Nachricht.txt" endresult
    
    print vInString
    print vWiederInInt
    print vEntschlüsselt
    print entschluesseltAscii
    print "---------"
    print result
    print result2
    print result3
    print result4
    print result5
    print endresult
    hClose handle



rsaSchluesseln textZahlen ed n = mod ((read textZahlen)^ed) n