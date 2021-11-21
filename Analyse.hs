import System.IO (getLine)
import Data.Char (toLower)
import GHC.Plugins (changeLast, bITMAP_BITS_SHIFT)


main = do
    putStrLn "Enter Text pls"
    entry <- getLine
    let lowCase = map toLower entry
        a = zaehlA lowCase 0
    print (entry ++ " hat " ++ show a ++ " a!")
    let b = returnBuchstaben a
        end = changeLetter lowCase b "a"
    putStrLn end


zaehlA :: [Char] -> Int -> Int
zaehlA [] n = n
zaehlA (l:ls) n
    | l == 'a' = zaehlA ls (n+1)
    | otherwise = zaehlA ls n

changeLetter :: [Char] -> [Char] -> [Char] -> [a]
changeLetter [] _ _ = []
changeLetter (l:ls) a b
    | l == a = b ++ changeLetter ls a b
    | otherwise = changeLetter ls a b

returnBuchstaben a
    | a > 9.78 = "E"
    | a > 7.55 = "N"
    | a > 7.27 = "I"
    | a > 7.00 = "S"
    | a > 6.51 = "R"
    | a > 6.15 = "A"
    | a > 5.08 = "T"
    | a > 4.76 = "D"
    | a > 4.35 = "H"
    | a > 3.44 = "U"
    | a > 3.06 = "L"
    | a > 3.01 = "C"
    | a > 2.53 = "G"
    | a > 2.51 = "M"
    | a > 1.89 = "O"
    | a > 1.89 = "B"
    | a > 1.66 = "W"
    | a > 1.21 = "F"
    | a > 1.13 = "K"
    | a > 0.79 = "Z"
    | a > 0.67 = "P"
    | a > 0.31 = "V"
    | a > 0.27 = "SZ"
    | a > 0.04 = "J"
    | a > 0.03 = "Y"
    | a > 0.02 = "X"
    | otherwise = "Q"