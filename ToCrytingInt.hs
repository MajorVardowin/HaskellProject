import Data.Char

{-
convertToCryptingInt nimmt einen String und konvertiert diesen
1. Alle Buchstaben in Binärzahlen
2. Je 2 Binärzahlen zu einer Zahl umgeformt
    // Bspw.: "Te" T = 84 = 01010100 => [0,0,1,0,1,0,1,0]; e = 115 = 01100101 => [1,0,1,0,0,1,1,0]
    //          => [0,0,1,0,1,0,1,0,1,0,1,0,0,1,1,0] = 25940
    // Aus "Test" wird so [25940,29811]
3. Alle Zahlen in ein Array geschrieben
-}


toInt :: String -> [Int]
toInt = map ord

toBinary :: Int -> [Int]
toBinary = helpF 8 [] where
    helpF 0 acc _ = reverse acc
    helpF n acc x = helpF (n-1) (bit:acc) x' where
        (x', bit) = x `divMod` 2


stringToBinary :: String -> [Int]
stringToBinary = concatMap toBinary.toInt

binaryToInt [] _ = 0
binaryToInt (l:ls) n = l*n + binaryToInt ls (n*2)

convertToCryptingInt [] = []
convertToCryptingInt ls = binaryToInt (take 16 (stringToBinary ls)) 1 : convertToCryptingInt (drop 2 ls)
{-
Es wird ein String erst in eine Liste aus Integer umgeformt, wobei ein Int aus jeweils 2 Buchstaben besteht
-}

