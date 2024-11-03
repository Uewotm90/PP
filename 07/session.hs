data Unary = Z | I Unary

one = I Z

two = I one

two' = I (I Z)

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

four = unary2int (I (I (I (I Z))))

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- least :: (Ord a) => Tree a -> a
-- least (Leaf x) = x
-- least (Node l x r) = min x (min (least l) (least r))

-- oak = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

data Aexp = Num Int | Var String | Plus Aexp Aexp | Mult Aexp Aexp

eval :: Aexp -> (String -> Int) -> Int
eval (Num n) _ = n
eval (Var x) ass = ass x
eval (Plus l r) ass = eval l ass + eval r ass
eval (Mult l r) ass = eval l ass * eval r ass

expr = Plus (Mult (Num 2) (Var "x")) (Var "y") -- should evaluate to 10 with the assignments [x->3,y->4]

assignments :: String -> Int
assignments "x" = 3
assignments "y" = 4

evaluated = eval expr assignments

data Encyclopedia a = Leaf String a | Node String a [Encyclopedia a] deriving (Show)

t1 = Node "mango" True [Node "dingo" False [Leaf "plip" True, Leaf "ninka" False], Leaf "plop" True, Node "plys" False [Leaf "boing" True]]

t2 = Node "plonk" 1 [Node "zap" 3 [Leaf "ninka" 8], Node "uhu" 4 [Leaf "gif" 9], Leaf "bingo" 5]

values :: Encyclopedia a -> [a]
values (Leaf _ v) = [v]
values (Node _ v vs) = v : concatMap values vs

maxVal :: (Ord a) => Encyclopedia a -> a
maxVal (Leaf _ v) = v
maxVal (Node _ v vs) = maximum (v : concatMap values vs)

layered :: (Ord a) => Encyclopedia a -> Bool
layered (Leaf _ _) = True
layered (Node _ x xs) = isLayered && all layered xs
  where
    isLayered = all (> x) (concatMap values xs)

-- isLayered = all (all (> x) . values) xs -- aparrently the preffered way of doing this

layeredNt = layered (Node "plonk" 4 [Node "zap" 3 [Leaf "ninka" 8], Node "uhu" 4 [Leaf "gif" 9], Leaf "bingo" 5])

layeredNt' = layered (Node "plonk" 1 [Node "zap" 3 [Leaf "ninka" 8], Node "uhu" 4 [Leaf "gif" 4], Leaf "bingo" 5])

class InVector a where
  (&&&) :: a -> a -> a
  (***) :: a -> a -> Int

instance InVector Bool where
  (&&&) :: Bool -> Bool -> Bool
  (&&&) = (||) 
  (***) :: Bool -> Bool -> Int
  (***) l r = if l && r then 1 else 0
