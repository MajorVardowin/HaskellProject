import Data.Char

{-
    Wieder in Schrift umwandeln!
    1. Man hat einen String Zahlen (siehe f), immer 2 Buchstaben wurden zu einer Zahl umgeform.
    2. durch convert werden diese Zahlen wieder in die 2 einzelnen Buchstaben aufgeteilt.
    3. flatten schreibt alle Zahlen hintereinander und entfernt verschachtelungen.
-}

-- konvertiert Zahl in Binär, dabei beachten [0,0,0,1] == 8 
myToBinary a
    | a > 0 = mod a 2 : myToBinary (div a 2)
    | otherwise = []

-- konvertiert Zahl in Integer
binaryToInt ls = helpToInt ls 1
    where
        helpToInt [] _ = 0
        helpToInt (l:ls) n = l*n + helpToInt ls (n*2)

-- Konvertiert die kombinierten Zahlen in 2 einzelne (ASCII) Werte
convert = map (toInt . myToBinary)
    where
        toInt [] = []
        toInt ls = binaryToInt (take 8 ls) : toInt (drop 8 ls)

-- BEISPIELE -- BEISPIELE -- BEISPIELE -- BEISPIELE -- BEISPIELE --

beispiel = map chr (concat (convert satz)) -- satz wird konvertiert, Liste 'flatted' und jedes Element in ein ASCII Zeichen konvertiert

satz = [25940,29811,21536,29541,116]

flatten = concat f

translate = map chr flatten

f = convert satz
f2 = map (20*) satz
c = convert f2
