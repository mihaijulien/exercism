module BinarySearch (find) where

import Data.Array

binarySearch :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int 
binarySearch array (left, right) x
  | right < left = Nothing       
  | x < val = binarySearch array (left, mid - 1) x
  | x > val = binarySearch array (mid + 1, right) x
  | otherwise = Just mid
  where
    mid = (left + right) `div` 2
    val = array ! mid
        

find :: Ord a => Array Int a -> a -> Maybe Int        
find array = binarySearch array (bounds array)