module Acronym (abbreviate) where

import Data.Char (isAlpha, isSpace, toUpper)

-- Replace hyphens with spaces
replaceHyphen :: Char -> Char
replaceHyphen c = if c == '-' then ' ' else c

-- Remove all non-alphabetic characters except spaces
removePunctuation :: String -> String
removePunctuation str = filter (\c -> isAlpha c || isSpace c) str

-- Generate the acronym from the cleaned string
abbreviate :: String -> String
abbreviate str = map (toUpper . head) (words (removePunctuation (map replaceHyphen str)))

