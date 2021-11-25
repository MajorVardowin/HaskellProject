
vigenereVer nachricht schluessel  =  vigenere nachricht schluessel "" 1
vigenereEnt nachricht schluessel  = vigenere nachricht schluessel "" (-1)


vigenere  nachricht  [] rest richtung  = []
vigenere []  schluessel rest richtung = []
vigenere nachricht schluessel [] richtung= vigenere nachricht schluessel schluessel richtung
vigenere nachricht schluessel rest richtung= (toEnum(erechnetWert (head nachricht) (head rest) richtung)::Char) : vigenere (tail nachricht) schluessel (tail rest) richtung

-- Haben es einmal für Großbuchstaben und einmal für Buchstaben gemacht
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

berechneAsciiZahl neuerWert  0 | neuerWert == 196 = 26
                             | neuerWert == 214 = 27
                             | neuerWert == 220 = 28
                             | otherwise = neuerWert -65

berechneAsciiZahl neuerWert  1 | neuerWert == 228 = 26
                             | neuerWert == 246 = 27
                             | neuerWert == 252 = 28
                             | neuerWert == 223 = 29
                             | otherwise = neuerWert -97


erzeugeAsciiBuchstabe neuerWert  0 | mod neuerWert 29 == 26 = 196
                                    | mod neuerWert 29 == 27 = 214
                                    | mod neuerWert 29 == 28  = 220
                                    | otherwise = 65 + mod neuerWert 29

erzeugeAsciiBuchstabe neuerWert  1 | mod neuerWert 30 == 26 = 228
                                    | mod neuerWert 30 == 27 = 246
                                    | mod neuerWert 30 == 28 = 252
                                    | mod neuerWert 30 == 29 = 223
                                    | otherwise = 97 + mod neuerWert 30


caesar :: [Char] -> Int -> Int -> [Char]
caesar wort verschiebung richtung = caesarTextZusammen wort (richtung * verschiebung) 


caesarTextZusammen :: [Char] -> Int -> [Char]
caesarTextZusammen [] verschiebung = []
caesarTextZusammen wort verschiebung = (toEnum (berechneCaesar (head wort) verschiebung)::Char) : caesarTextZusammen (tail wort) verschiebung


berechneCaesar :: Char -> Int -> Int
berechneCaesar a b  | zahlA >= 65 && zahlA <= 90 = mod (zahlA - 65 + b) laengeAlphabet + 65
                    | zahlA >= 97 && zahlA <= 122 = mod (zahlA - 97 + b) laengeAlphabet + 97
                    | otherwise = zahlA
    where zahlA = fromEnum a
          laengeAlphabet = 26