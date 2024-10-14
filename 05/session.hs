import Prelude hiding (replicate)
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:replicate (n-1) x

replicateEx = replicate 3 5 -- should give [5,5,5]

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x:xx:xs) = x:improve xs

improveEx = improve [1,2,3,4,5,6,7] -- should give [1,3,5,7]

