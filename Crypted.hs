import MyPrimeFunctions
import MyMathFunctions

q = 7
p = 11
e = 13

berechnungN = p * q
berechnungM = (p-1) * (q-1)

get2ndOf3 (_,d,_) = d

getPrivate = get2ndOf3 (MyMathFunctions.extendedEuclidean e berechnungM) + berechnungM

checkE = MyPrimeFunctions.check berechnungM e

{-Programmablauf-}
a = "Test"
main = do
    putStrLn "Input: p"
    p <- getLine
    putStrLn "Input: q"
    q <- getLine
    putStrLn "Input: e"
    e <- getLine
    --n <- p*q
    --m <- (p-1) * (q-1)
    test   

test = putStrLn a