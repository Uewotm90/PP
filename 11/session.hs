import Data.List

-- import Prelude hiding (fmap)
tuple :: (Monad m) => m a -> m b -> m (a, b)
tuple mx my =
  mx >>= \x ->
    my >>= \y -> return (x, y)

tuple' :: (Monad m) => m a -> m b -> m (a, b)
tuple' mx my = do
  x <- mx
  y <- my
  return (x, y)

tupled = tuple (Just 1) (Just True)

tupled2 = tuple (Just 1) Nothing

tupled' = tuple' (Just 1) (Just True)

tupled2' = tuple' (Just 1) Nothing

exp z s f = do
  y <- z
  s y
  return (f y)

exp' z s f =
  z >>= \y ->
    s y >> return (f y)

-- -- test :: Monad m => m a -> (a -> b) -> m b
-- test mx f = do
--     x <- mx
--     return (f x)

-- fourfirst returns a list (monad) of pairs: (4,x) where x is an element in the list
-- equivalent to
-- fourfirst xs =[(4,x)|x<-xs]
-- fourfirst [1,2,3,4,5] = [(4,1),(4,2),(4,3),(4,4),(4,5)]

data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l r) = flatten l ++ flatten r

-- toTree :: [a] -> Tree a
-- -- toTree [] = -- not defined
-- toTree xs =

-- instance Functor Tree where
--   fmap :: (a -> b) -> Tree a -> Tree b
--   fmap g (Leaf a) = Leaf (g a)
--   fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- instance Applicative Tree where
--   pure :: a -> Tree a
--   pure = Leaf
--   (<*>) :: Tree (a -> b) -> Tree a -> Tree b
--   g <*> t = flatten g <*> flatten t
-- issort :: Ord a => [a] -> Bool
-- issort xs = xs == sort xs

-- returns the left and rightermost element in a tree
minmax :: (Ord a) => Tree a -> Maybe(a, a)
minmax (Leaf x) = Just (x, x)
minmax (Node l r) = do
    (x1,y1) <- minmax l
    (x2,y2) <- minmax r
    if y1<=x2 then return (x1,y2) else Nothing
--     (left, right)
--   where
--     left = fst $ minmax l
--     right = snd $ minmax r

minorder :: (Ord a) => Tree a -> Maybe a
minorder (Leaf a) = Just a
minorder t = do
    (x,_) <- minmax t
    return x
--   l <- minorder ln
--   r <- minorder rn
--   if snd (minmax ln) <= fst (minmax) then return l else Nothing -- seems to be working without even using minmax



  -- isOrd (minmax ln) (minmax rn)
  -- where
  --     isOrd (a,b) (c,d) = booltomaybe (issort [a,b,c,d])
  --     booltomaybe True = Just 1
  --     booltomaybe False = Nothing

oak = Node (Node (Leaf 5) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

oak' = Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 1) (Leaf 2)) -- subtrees are minordered

oakOrd = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))


tree = Node (Node (Leaf "aha") (Leaf "dingo")) (Leaf "plip")
treeunord = Node (Node (Leaf "aha") (Leaf "plip")) (Leaf "dingo")