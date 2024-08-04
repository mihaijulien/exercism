module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n < 1 = Nothing
    | otherwise = Just (toInteger ([x | x <- [2..], isPrime x] !! (n-1)))


isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..n-1], n `mod` x == 0]