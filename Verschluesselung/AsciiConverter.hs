module AsciiConverter
(
    asciiIntToString, asciiStringtoInt
)
where
import Data.Char

asciiIntToString :: Integer -> [Char]
asciiIntToString zahl  
        | zahl < 0 = "" -- Sicherheitsfall (unwahrscheinlich)
        | zahl > 255 = asciiIntToString (div zahl 255) ++  [chr (mod intZahl 255)] 
        | otherwise = [chr intZahl]
                where   intZahl :: (Integral a) => a
                        intZahl = fromInteger zahl
-- (redundanterAufruf (zahl / 255)) ++ mod intZahl 255
-- [letztesModIntZahl, ... , erstesModIntZahl]


asciiStringtoInt :: Enum a => [a] -> Int
asciiStringtoInt zahl =  asciiStringtoIntRechner zahl 0
asciiStringtoIntRechner :: (Integral t, Enum a) => [a] -> t -> Int
asciiStringtoIntRechner [] x = 0
asciiStringtoIntRechner zahl x  =  umwandelnInZahl (last zahl) * 255^ x + asciiStringtoIntRechner (init zahl) (x+1)
-- asciiStringtoInt [Eingabe]
-- asciiStringtoIntRechner [Eingabe] 0 = um
-- [ABC] = 67*255^0 + 66*255^1 + 65*255^2
-- A = 65, B = 66, C = 67

umwandelnInZahl ch = fromEnum ch
-- ASCII Wert pro Char ch

umwandelnInBuchstabe zahl = toEnum zahl


{-
woerterAneinanderKlatschen [] = ""
woerterAneinanderKlatschen (satz) =  laengeAnpassen (umwandelnInZahl ( head satz)) ++ (woerterAneinanderKlatschen (tail satz))

wandelInZeichen zahl  | zahl <= 0 = do ""
        | mod zahl 1000 <= 255 =  wandelInZeichen (div intZahl 1000) ++ [toEnum(mod intZahl 1000)]
        | otherwise = wandelInZeichen (div intZahl  100) ++ [toEnum(mod intZahl 100)]
                 where  intZahl :: (Integral a) => a
                        intZahl = fromInteger zahl

laengeAnpassen zahl =  if zahl < 100 then ('0':show zahl) else show(zahl)
-}