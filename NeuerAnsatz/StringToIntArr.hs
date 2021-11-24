module StringToIntArr
(
    nimm
)
where

deleteStrich3 [] = 0
deleteStrich3 eingabe   | head eingabe == ',' = 0
                        | head eingabe == '[' = deleteStrich3 (tail eingabe)
                        | head eingabe == ']' = 0
                        | otherwise = 1 + deleteStrich3 (tail eingabe)
                        --else 0 --if head eingabe /= ',' || head eingabe /= '[' || head eingabe /= ']' then 1 + deleteStrich3 (tail eingabe)
                        --else 0


nimm [] = []
nimm eingabe    | head eingabe == ',' || head eingabe == '[' || head eingabe == ']' = nimm (tail eingabe)
                | otherwise = (read(take ein eingabe)::Int) : nimm (drop ein eingabe)
                where ein = deleteStrich3 eingabe

