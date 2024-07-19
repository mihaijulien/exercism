module Phone (number) where

number :: String -> Maybe String
number str = cleanNumber str >>= formatCountryCode >>= formatAreaCode >>= formatExchangeCode

type PhoneNumber = String

cleanNumber :: PhoneNumber -> Maybe PhoneNumber
cleanNumber text = Just (filter (\x -> elem x ['0'..'9']) text)

formatCountryCode :: PhoneNumber -> Maybe PhoneNumber
formatCountryCode num 
     | length num == 10 = Just num
     | length num == 11 = if head num == '1' then Just (tail num) else Nothing
     | otherwise = Nothing

formatAreaCode :: PhoneNumber -> Maybe PhoneNumber
formatAreaCode num 
     | all (`elem` ['2'..'9']) (take 3 num) = Just num
     | otherwise = Nothing

formatExchangeCode :: PhoneNumber -> Maybe PhoneNumber
formatExchangeCode num
     | all (`elem` ['2'..'9']) (take 3 (drop 3 num)) = Just num
     | otherwise = Nothing
