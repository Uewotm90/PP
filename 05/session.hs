import Prelude hiding (replicate)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

replicated = replicate 3 5 -- should give [5,5,5]

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x : _ : xs) = x : improve xs

improved = improve [1, 2, 3, 4, 5, 6, 7] -- should give [1,3,5,7]

-- | hej
fizzbuzz :: Int -> String
fizzbuzz x | (x `mod` 15) == 0 = "FizzBuzz"
fizzbuzz x | (x `mod` 3) == 0 = "Fizz"
fizzbuzz x | (x `mod` 5) == 0 = "Buzz"
fizzbuzz x = show x
-- ^ med dig

fizz = fizzbuzz 3

buzz = fizzbuzz 5

bussin = fizzbuzz 15

-- medio session
rev' :: [a] -> [a]
rev' [] = []
rev' (x : xs) = rev' xs ++ [x]

revd = rev' [1, 2, 3, 4, 5]

descending :: (Ord a) => [a] -> Bool
descending [] = True
descending [x] = True
descending (x : xs) = x >= head xs && descending xs

descended = descending revd

isolate :: (Eq a) => [a] -> a -> ([a], [a])
isolate [] _ = ([], [])
isolate [l] x = if x == l then ([], [l]) else ([l], [])
isolate (l : ls) x = if x == l then (l1, l : l2) else (l : l1, l2)
  where
    (l1, l2) = isolate ls x

isolated = isolate [4, 5, 4, 6, 7, 4] 4

wrapup :: (Eq a) => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x : xs) | x == z = (x : zz) : last
  where
    ((z : zs) : rest) = wrapup xs
    zz = if x == z then z : zs else []
    last = wrapup (concat rest)
wrapup (x : y : xs) = [x] : last
  where
    last = wrapup (y : xs)

wrapupped = wrapup [1, 1, 1, 2, 3, 3, 2]

-- triples :: [(a, a, a)] -> ([a], [a], [a])
-- triples [] = ([],[],[])
-- triples

-- triples [] = ([],[],[])
-- triples [(a, b, c)] = ([a], [b], [c])
-- triples 
-- triples (x : xs, y : ys, z : zs) = [x, y, z] : Triples [(xs, ys, zs)]

-- wrapup :: (Eq a) => [a] -> [[a]]
-- wrapup [] = []
-- wrapup [x] = [[x]]
-- wrapup (x : y : xs) | x == y = if y == z then [[x, y]++z:zs, wrapup (concat rest) ] else [[x,y], concat (wrapup xs)]
--   where
--     ((z : zs) : rest) = wrapup xs
-- wrapup (x : y : xs) | otherwise = []

-- wrapup (x:y:xs) = if x == y then [[x,y],rest] else [[x],[y],rest]
--     where
--         rest =

-- bogstavsopgaver