module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
    | n == 1 = Just 0
    | n <= 0 = Nothing
    | even n = fmap (+1) (collatz (div n 2))     
    | otherwise = fmap (+1) (collatz(3*n + 1))
