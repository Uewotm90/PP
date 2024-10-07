onlytwo :: [a] -> Bool
onlytwo [_,_] = True
onlytwo _ = False


dotp :: Num a => (a,a) -> (a,a) -> a
dotp x y = fst x * fst y + snd x * snd y


dotp' :: Num a => ((a,a),(a,a)) -> a
dotp' x = fst (fst x) * fst (snd x) + snd (fst x) * snd (snd x)

alldots :: Num a => [(a,a)] -> [(a,a)] -> [a]
-- alldots xs ys = [(x,y) | x <- xs, y <- ys]
alldots xs ys = map dotp' combinations
                where combinations =  [(x,y) | x <- xs, y <- ys]

oneonepair = (1,1)
oneonepairlist = [oneonepair, oneonepair]

example = alldots [(1,2),(3,4)] [(5,6),(7,8)]