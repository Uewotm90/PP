nsonly n = map (* n) [0 ..]

tihi = length $ nsonly 2

-- not a lot of recursion here :D
nsonly' n = n * 0 : map (* n) [1 ..]

-- evaluates to 17 AND terminates since f (nonterminating) is never evaluated
plip = fst (17, f 484000)
  where
    f x = f x + 1

x = 1 : (map (1 +) x)

-- take 5 x
--  = take 5 (1:(2:map (1+)x))
--  = take 5 (1:(2:(3:(map (1+) x))))

-- take 5 x
-- = take 5 (1:map (1+) x)
-- = take 5 (1:(2:map (1+) x))
-- = take 5 (1:(2:()))

fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibsfrom n1 n2 = n1 + n2 : fibsfrom n2 (n1 + n2)

fib' n = take n $ 1 : (1 : fibsfrom 1 1)

-- a = takeWhile (<2^31) $ fib' 500

-- indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x : y : ys) = x : (e : (indflet e (y : ys)))

-- the pattern (x:y:ys) fails when splitting 1:(2:undefined) (splitting undefined into (x:y:ys))
troll = head (indflet 1 (2:undefined))


-- indflet' _ [] = []
-- indflet' _ [x] = [x]
-- indflet' e (x:ys) = x:e:indflet  e ys

zeroes = "0":(map ("0"++) zeroes)
ones = "1":(map ("1"++) ones)

allbinaries = [(++)] <*> zeroes <*> ones


-- allbinaries' = do
--     zero <- zeroes
--     -- one <- ones
--     return ([zero ++ one])

-- allbinaries'' = [zero ++ one | zero <- zeroes, one <- ones] 

-- allbinaries''' = l 0
--     where 
--         l n = 