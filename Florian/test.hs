
laengeAnpassen zahl =  if zahl < 100 then ('0':show zahl) else show(zahl)

umwandelnInBuchstabe zahl = toEnum zahl

umwandelnInZahl ch = fromEnum ch


woerterAneinanderKlatschen [] = ""
woerterAneinanderKlatschen (satz) =  laengeAnpassen (umwandelnInZahl ( head satz)) ++ (woerterAneinanderKlatschen (tail satz))

nimmDrei satz =  read satz  :: Int

zAA a = zahlenAneinanderKlatschen a 1
zahlenAneinanderKlatschen []  c = 0
zahlenAneinanderKlatschen a c = fromEnum (last  a) * c + zahlenAneinanderKlatschen (init a)  (c*1000)
