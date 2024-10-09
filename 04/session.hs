-- Functions and lists (incl. list comprehensions)

onlytwo :: [a] -> Bool
onlytwo [_,_] = True
onlytwo _ = False


dotp :: Num a => (a,a) -> (a,a) -> a
dotp x y = fst x * fst y + snd x * snd y

-- couldnt be bothered to zip
dotp' :: Num a => ((a,a),(a,a)) -> a
dotp' x = fst (fst x) * fst (snd x) + snd (fst x) * snd (snd x)

alldots :: Num a => [(a,a)] -> [(a,a)] -> [a]
-- alldots xs ys = [(x,y) | x <- xs, y <- ys]
alldots xs ys = map dotp' combinations
                where combinations =  [(x,y) | x <- xs, y <- ys]

oneonepair = (1,1)
oneonepairlist = [oneonepair, oneonepair]

example = alldots [(1,2),(3,4)] [(5,6),(7,8)]

-- 1. opgave
idhead :: Eq a => [(a,a)] -> Bool
idhead (x:_) = uncurry (==) x


-- 2. opgave

ispyt :: (Ord a, Integral a) => a -> a -> a -> Bool
ispyt a b c = (a <= b && b<c) && a^2+b^2==c^2


pyt :: Integral a => a -> [(a,a,a)]
pyt k =  [(a,b,c) | c <- [1..k], a <- [1..k-1],b <- [1..k-1], ispyt a b c] --broken


--3. opgave
bighead :: (Ord a, Num a) => [a] -> Int
bighead (x:xs) = length [t | t <- xs, t > x]


-- 4. opgave
-- plonk x y z = x+y+z
plonk = \x -> (\y -> (\z -> x+y+z))
-- \ x y z -> x+y+z (alternatively)

--5. opgave

isperfect :: Integral a => a -> Bool
isperfect x = sum [factor | factor <- [1..x-1],x `mod` factor == 0] == x


-- additional

sevens k = [number | number <- [1..k-1], number `mod` 7 == 0]

flop xs = [(b,a)| (a,b) <- xs]

dupli :: [a] -> [a]
dupli (x:xs) = x:x : dupli xs
dupli [] = []

sums m n = [ x+y | x <- [ 1..m], y <- [ 1..n ] ]

sums' m n = concat [[x+y | x <- [1..m]] | y <- [1..n]]