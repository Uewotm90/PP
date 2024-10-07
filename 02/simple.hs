-- This is the simple program from the slides from the introduction
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}

laengde :: (Num p) => [a] -> p

laengde [] = 0
laengde (x:l) = 1 + (laengde l)

myList = [2,3,17,9,69,484000]

data BTree = BLeaf Int | BBranch Int BTree BTree deriving Show

-- sumtree :: BTree -> Int

sumtree (BLeaf x) = x
sumtree (BBranch x t1 t2) = let v1 = sumtree t1
                                v2 = sumtree t2
                            in x + v1 + v2


myBigOak = BBranch 14 (BLeaf 13) (BLeaf 17)

-- Quicksort

qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]


-- partial: not defined for lists with size <2
second :: [a] -> a
second x = x !! 1 

-- total: defined for lists of any size
second' :: [a] -> Maybe a
second' x | laengde x >=2 = Just (x !! 1)
          | otherwise = Nothing

allbutsecond :: [a] -> [a]

-- allbutsecond (x:y:z:xs) = x:xs
allbutsecond (x:y:xs) = x:xs

allbutsecond' :: [a] -> [a]
allbutsecond' xs = y:z
                where y = head xs
                      z = tail (tail xs)

midtover :: [a] -> ([a],[a])
midtover x = (a,b)
            where a = take (laengde x `div` 2) x
                  b = drop (laengde x `div` 2) x

-- midtover' :: [a] -> ([a],[a])
-- midtover' [] = ([],[])
-- midtover' [x] ([],[x])
-- midtover' [x,y] ([x],[y])
-- midtover' (x:xs) =  (x:y,z:zs)
--                 where mid =
--                       y = 
--                       z = 
--                       zs = last xs