module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | n < 1 = Nothing
    | isPerfect n = Just Perfect
    | isAbundant n = Just Abundant
    | isDeficient n = Just Deficient
    | otherwise = Nothing


isPerfect :: Int -> Bool
isPerfect n = calculateAliquotSum n == n

isAbundant :: Int -> Bool
isAbundant n = calculateAliquotSum n > n

isDeficient :: Int -> Bool
isDeficient n = calculateAliquotSum n < n

calculateAliquotSum :: Int -> Int
calculateAliquotSum n = sum (divisors n)
    where
      divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]