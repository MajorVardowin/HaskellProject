module Crypted
(
  main
) where
import MyPrimeFunctions
import MyMathFunctions
import ToCryptingInt
import InToString

q = 7
p = 11
e = 13

berechnungN = p * q
berechnungM = (p-1) * (q-1)

get2ndOf3 (_,d,_) = d

getPrivate = get2ndOf3 (MyMathFunctions.extendedEuclidean e berechnungM) + berechnungM

checkE = MyPrimeFunctions.check berechnungM e

{-Programmablauf-}
main = do
  putStrLn "Input: Test String"
  str <- getLine
  let
    a = convertToCryptingInt str -- der eingegebene String wird in ein Array aus Int konvertiert
  print a
  let 
    b = mainConvert a -- Array aus Zahlen wird in String umgeformt
  print b
  putStrLn "Ende"