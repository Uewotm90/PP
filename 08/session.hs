first a b xss = all (\c -> (a - a) > b && c) (concat xss) -- close enough :)

second (f, b) a = f b + a

third f g h a = h (recip (f a) / recip (g a))

triples :: (Num a) => [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples [(a, b, c)] = ([a], [b], [c]) -- maybe unnecesarry
triples ((x,y,z):rest) = (x:xs,y:ys,z:zs)
    where
        (xs,ys,zs) = triples rest


frequencies :: String -> [(Char, Int)]
-- who needs base cases
-- frequencies [] = []
-- frequencies [a] = [(a,1)]
frequencies (a:as) = dedupUpdate ((a,1):frequencies as)
    where
        -- dedup :: [(Char,Int)] -> [(Char,Int)]
        -- dedup [a] = [a]
        dedupUpdate (x:xs) = (fst x, snd x +length (filter predicate xs)):dedupUpdate (filter (not . predicate) xs)
            where
                predicate y = y == x
