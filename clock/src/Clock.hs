module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock { hours :: Int
                    , mins :: Int}
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock ((h + (m `div` 60)) `mod` 24) (m `mod` 60)

toString :: Clock -> String
-- toString (Clock h m) = show h ++ ":" ++ show m <---- doesn't properly show time eg. Expected '"08:00"' but got '"8:0"'
toString (Clock h m) = printf "%02d:%02d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m (Clock h2 m2) = fromHourMin (h + h2) (m + m2)
           