module Bob (responseFor) where

import Data.Char
import qualified Data.Text as T

responseFor :: String -> String
responseFor input 
    | isNothing = "Fine. Be that way!"
    | isQuestion && isShouting = "Calm down, I know what I'm doing!"    
    | isQuestion = "Sure."
    | isShouting = "Whoa, chill out!"      
    | otherwise = "Whatever." 
  where
    text = T.unpack (T.strip (T.pack input)) -- ending with whitespace testcase
    isQuestion = last text == '?'
    isShouting = all isUpper letters && any isUpper letters
    letters = filter isLetter text 
    isNothing = all isSpace text
