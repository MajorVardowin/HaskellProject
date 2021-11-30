module AsciiConverter
(
    asciiIntToString, asciiStringtoInt
)
where
import Data.Char

-- Wird mit Int-Wert aufgerufen, dieser muss in einen String umgerechnet werden
asciiIntToString :: Integer -> [Char]
asciiIntToString zahl  
        | zahl < 0 = "" -- Sicherheitsfall (unwahrscheinlich)
        | zahl > 255 = asciiIntToString (div zahl 255) ++  [chr (mod intZahl 255)] 
        | otherwise = [chr intZahl]
                where   intZahl :: (Integral a) => a
                        intZahl = fromInteger zahl
-- (rekursiverAufruf (zahl / 255)) ++ chr (mod intZahl 255)
-- [letztesModIntZahl, ... , erstesModIntZahl]


-- Wird mit String aufgerufen, dieser muss in einen Int-Wert umgerechnet werden
asciiStringtoInt :: Enum a => [a] -> Int
asciiStringtoInt str =  asciiStringtoIntRechner str 0

-- Rechnet einen String in einen Int-Wert um
asciiStringtoIntRechner :: (Integral t, Enum a) => [a] -> t -> Int
asciiStringtoIntRechner [] x = 0
asciiStringtoIntRechner str x  =  umwandelnInZahl (last str) * 255^ x + asciiStringtoIntRechner (init str) (x+1)
-- asciiStringtoInt [Eingabe]
-- asciiStringtoIntRechner [Eingabe] 0 = um
-- "ABC" = 67*255^0 + 66*255^1 + 65*255^2
-- A = 65, B = 66, C = 67


-- ASCII Wert pro Char ch berechnen
umwandelnInZahl ch = fromEnum ch

-- ASCII Wert in Buchstaben umformen
umwandelnInBuchstabe zahl = toEnum zahl
