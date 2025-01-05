-- 1.1

-- 1.2
allAnswers f xs =
  let list = map f xs
   in if all (/= Nothing) list
        then Just list
        else Nothing

-- 1.3
allAnswers' f [] = Just []
allAnswers' f (x : xs) = do
  fx <- f x
  fxs <- allAnswers' f xs
  return (fx : fxs)

-- 2.1a
a x (u, v) = if u == v then [x] else []

-- 2.1b
b f p l r = if p && l == r then f l r else False

-- 2.1c
c v mb = do
  putStr (show v)
  return mb

-- 2.1d
d f l r = [f l, f r]

-- 2.2
-- functions a,c and d are parametrically polymorphic while functions a, b and c are ad hoc polymorphic

-- 3.1
data Tree a = Leaf a | Node (Tree a) (Tree a)

figureOne = Node (Leaf "dog") (Node (Leaf "cat") (Leaf "hamster"))

-- 3.2
minimax t =
  let list = treeToList t
   in (minimum list, maximum list)
  where
    treeToList (Leaf e) = [e]
    treeToList (Node l r) = treeToList l ++ treeToList r

-- By converting the tree to a list, we can utilize the already inbuilt minimum and maximum functions
-- 4.1
echo =
  putStr "Please type a word: " >> getLine >>= \x ->
    putStrLn ("You typed " ++ x)

-- 4.2
boolPairs i = read i :: [(Bool, Bool)]

seconds = do
  input <- getLine
  putStrLn $ show $ map snd $ boolPairs input
  return ()

-- [(True,False),(False,True),(False,False)]

-- 5.1
-- Alternating lists (using shown syntax) is disallowed since list elements in Haskell can only be of one type

-- 5.2'
data Alternating' a b = Pair' [(a, b)] (Maybe a)

-- The 'Maybe a' is present in case a list of odd length is required
myalt' = Pair' [(5, True), (6, False), (7, False)] Nothing

-- 5.3'
separate' (Pair' [] Nothing) = ([], [])
separate' (Pair' [] (Just v)) = ([v], [])
separate' (Pair' ((x, y) : xs) r) = ((x : left) ++ maybeToList r, y : right)
  where
    (left, right) = separate' (Pair' xs Nothing)
    maybeToList Nothing = []
    maybeToList (Just v) = [v]

-- 5.4'
infinite' = Pair' (iterate (\(x, y) -> (x + 1, y ++ "a")) (1, "a")) Nothing

-- 6
newtype ToPairs a = TP (a, a)

-- 6.1
booltopairs = TP (True, False)

ftoPairs = TP (\x -> if x == Just 1 then 0 else 1, \y -> if y == Just 0 then 1 else 0)

-- where f mx = if mx == Nothing then 0 else 1

instance Functor ToPairs where
  --   fmap :: (a -> b) -> ToPairs a -> ToPairs b
  fmap g (TP (l, r)) = TP (g l, g r)

instance Applicative ToPairs where
  --   pure :: a -> ToPairs a
  pure v = TP (v, v)

  --   (<*>) :: ToPairs (a -> b) -> ToPairs a -> ToPairs b
  (<*>) (TP (g, _)) (TP (l, r)) = TP (g l, g r)