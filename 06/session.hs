import Distribution.Simple.Utils (xargs)

-- prep

charToInt :: Char -> Int
charToInt 'a' = 1
charToInt x = charToInt (pred x) + 1

positions :: String -> [Int]
positions xs = map charToInt xs

-- alternatively
-- positions :: String -> [Int]
-- positions = map charToInt

sumsq :: Int -> Int
sumsq n = foldr (\c acc -> acc + c ^ 2) 0 [1 .. n]

sumsqfour = sumsq 4

sumsqniner = sumsq 9

-- problem set

dbs :: (Eq a, Num a) => [(a, a)] -> [(a, a)]
dbs xs = filter (\p -> fst p * 2 == snd p) xs

dbsexample = dbs [(1, 4), (3, 6), (9, 2), (4, 8)]

within :: (Num a, Ord a) => [a] -> (a, a) -> [a]
within xs (a, b) = filter inrange xs
  where
    inrange p = p >= a && p <= b

someWithin = within [1, 3, 4, 5, 2] (1, 3)

notWithin = within [1, 3, 4, 5, 2] (3, 1)

sumrows :: (Num a) => [[a]] -> [a]
sumrows xss = map sum xss

sumthing = sumrows [[1, 2], [3, 4]]

fact k = product [1 .. k]

approx :: Int -> Double
approx n = foldr (+) 0 (take n facts)
  where
    facts = [1 / fact k | k <- [0 ..]]

approx' :: Int -> Double
approx' n = sum (take n facts)
  where
    facts = [1 / fact k | k <- [0 ..]]

-- letters

partitionf :: (a -> Bool) -> [a] -> ([a], [a])
partitionf p xs = (inde, ude)
  where
    inde = filter p xs
    ude = filter (not . p) xs

partitioned = partitionf (> 5) (take 10 [1 ..])

-- partitionfr :: (a -> Bool) -> [a] -> ([a],[a])
-- partitionfr p xs = foldr part ([],[]) xs
--     where
--         part :: a -> ([a],[a]) -> ([a],[a])
--         part x acc = if p x then (fst x:acc,snd acc) else (fst acc, snd x:acc)

partitionfr :: (a -> Bool) -> [a] -> ([a], [a])
partitionfr p xs = (inde, ude)
  where
    inde = foldr (\x acc -> if p x then x : acc else acc) [] xs
    ude = foldr (\y acc -> if (not . p) y then y : acc else acc) [] xs

partitionedfrfrnocap = partitionf (> 5) (take 10 [1 ..])

filterfr :: (a-> Bool) -> [a] -> [a]
filterfr p xs = 