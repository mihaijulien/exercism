module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, fromList, adjust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = Right empty
nucleotideCounts dnaStr = countAndUpdateMap dnaStr nucleotideMap
  where
    nucleotideMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    
    countAndUpdateMap [] counts = Right counts
    countAndUpdateMap (x:xs) counts
      | x == 'A' = countAndUpdateMap xs (adjust (+1) A counts)
      | x == 'C' = countAndUpdateMap xs (adjust (+1) C counts)
      | x == 'G' = countAndUpdateMap xs (adjust (+1) G counts)
      | x == 'T' = countAndUpdateMap xs (adjust (+1) T counts)
      | otherwise = Left "Invalid nucleotide"