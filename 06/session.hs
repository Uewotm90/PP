-- prep

charToInt :: Char -> Int
charToInt 'a' = 1
charToInt x = charToInt (pred x) +1

positions :: String -> [Int]
positions xs = map charToInt xs

-- alternatively
-- positions :: String -> [Int]
-- positions = map charToInt


sumsq :: Int -> Int
sumsq n = foldr (\c acc -> acc + c^2) 0 [1 .. n]

sumsqfour = sumsq 4
sumsqniner = sumsq 9
-- problem set


-- bonus