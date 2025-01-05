-- problem 1.1
rotate (x : xs) = xs ++ [x]

-- the function is parametrically polymorhpic. No type constraints are provided, hence the function will work for lists of any type

-- problem 1.2
allrotates xs = xs : take (length xs - 1) (allrotates (rotate xs))

-- This function is also parametrically polymorphic since type a can be substituted for any type

-- problem 1.3
allrotates' xs = map (\x -> iterate rotate xs !! x ) [0..length xs-1]
-- allrotates' xs = take (length xs) $ iterate rotate xs
-- problem 2.1
data Tree a = Leaf a | Node (Tree a) (Maybe a) (Tree a) deriving (Show)

-- Tree t1
t1 = Node (Node (Leaf 17) Nothing (Leaf 484000)) Nothing (Leaf 1964)

-- Tree t2
t2 = Node (Leaf "plip") (Just "bingo") (Node (Leaf "uhu") (Just "plop") (Leaf "fedtmule"))

-- Problem 2.2
isfull (Leaf _) = True
isfull (Node _ Nothing _) = False
isfull (Node l _ r) = isfull l && isfull r

-- Problem 2.3
preorder (Leaf x) = Just [x]
preorder (Node l x r) = do
  y <- x
  left <- preorder l
  right <- preorder r
  return (y : (left ++ right))

-- Problem 3.1
remove xs ys = [y | y <- ys, y `notElem` xs]

-- Problem 3.2
remove' [] ys = ys
remove' (x : xs) ys = remove xs $ filter (/= x) ys

-- Problem 4
newtype WrapString a = WS (a,String) deriving Show

instance Functor WrapString where
    fmap :: (a -> b) -> WrapString a -> WrapString b
    fmap f (WS (x,s)) = WS (f x,s)

-- Problme 4.1
instance Applicative WrapString where
    pure :: a -> WrapString a
    pure x = WS (x,"")
    (<*>) :: WrapString (a -> b) -> WrapString a -> WrapString b
    (<*>) (WS (g,_)) (WS (x,s)) = WS (g x,s)

instance Monad WrapString where
    return = pure
    (>>=) :: WrapString a -> (a -> WrapString b) -> WrapString b
    (>>=) (WS (x,s)) g = g x

-- Problem 4.3    
-- pairup :: WrapString a -> WrapString b -> WrapString (a,b)
pairup (WS (x,s)) rws = do
    y <- rws
    WS ((x,y),s)

-- Problem 5.1
fivepointone x y z = if x+y<z then (x,y) else (z,y)
-- this function is ad hoc polymorhpic, since the types of x, y and z is constrained to Ord and Num by the functions (+) and (<)

-- Problem 5.2
fivepointtwo = [(toInteger 1, \x -> 'c')]
-- This expression is parametrically polymorphic since lambda expression accepts arguments of any type

-- Problem 5.3
fivepointthree f x = f x True
-- This function is parametrically polymorphic since types t1 and t2 are unconstrained

-- Problem 5.4
fivepointfour x = map (1+) [x..] -- not quite there
-- ! fivepointfour' = fromEnum $ toEnum undefined
-- This function is ad hoc polymorphic since the type x is constrained to the typeclasses Num and Enum

-- Problem 6.1
naturals = 1:map (+1) naturals
-- Problem 6.2
facs = map (\x -> product [1..x]) [0..]
-- firstten = take 10 facs
-- Problem 6.3
facs' = 1 : zipWith (*) facs' naturals
-- firstten' = take 10 facs'