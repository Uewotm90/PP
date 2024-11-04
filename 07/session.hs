import Data.Maybe
import Data.Maybe (listToMaybe)

data Unary = Z | I Unary

one = I Z

two = I one

two' = I (I Z)

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

four = unary2int (I (I (I (I Z))))

data Tree a = Leaf a | Node (Tree a) a (Tree a)

least :: (Ord a) => Tree a -> a
least (Leaf x) = x
least (Node l x r) = min x (min (least l) (least r))

oak = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

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

data Encyclopedia a = Leaf' String a | Node' String a [Encyclopedia a] deriving (Show)

t1 = Node' "mango" True [Node' "dingo" False [Leaf' "plip" True, Leaf' "ninka" False], Leaf' "plop" True, Node' "plys" False [Leaf' "boing" True]]

t2 = Node' "plonk" 1 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 9], Leaf' "bingo" 5]

values :: Encyclopedia a -> [a]
values (Leaf' _ v) = [v]
values (Node' _ v vs) = v : concatMap values vs

-- maxVal :: (Ord a) => Encyclopedia a -> a
-- maxVal (Leaf' _ v) = v
-- maxVal (Node' _ v vs) = maximum (v : concatMap values vs)

minLayer :: (Ord a) => [Encyclopedia a] -> a
minLayer [Leaf' _ v] = v
minLayer [Node' _ v _] = v
minLayer ((Leaf' _ v) : es) = min v (minLayer es)
minLayer ((Node' _ v vs) : es) = min v (minLayer es)

layer :: [Encyclopedia a] -> [a]
layer [Leaf' _ v] = [v]
layer [Node' _ v _] = [v]
layer ((Leaf' _ v) : es) = v : layer es
layer ((Node' _ v vs) : es) = v : layer es

children :: Encyclopedia a -> [a]
children (Leaf' _ _) = []
children (Node' _ _ vs) = layer vs
-- maximum v:(map layer vs)

layered :: (Ord a) => Encyclopedia a -> Bool
layered (Leaf' _ _) = True
layered (Node' _ x xs) = isLayered && all layered xs
  where
    isLayered = all (> (x)) (concatMap values xs)

layered' :: (Ord a) => Encyclopedia a -> Bool
layered' (Leaf' _ _) = True
layered' (Node' _ x xs) = all (> x) (layer xs) && all layered' xs

-- layered' (Node' _ x xs) = x < minLayer xs && all layered' xs

layered'' :: (Ord a) => [Encyclopedia a] -> Bool
layered'' [Leaf' _ _] = True
layered'' [Node' _ x es] = all (> x) (layer es) && layered'' es
layered'' (e : es) = layered'' [e] && layered'' es

-- isLayered = all (all (> x) . values) xs -- aparrently the preffered way of doing this

-- layered''' :: Ord a => [Encyclopedia a] -> Maybe a -> Bool
-- layered''' [e] a = isNothing a || (all (\x -> x > fromJust a ) (layer [e])) && layered'''
layered'''' :: (Ord a) => Maybe a -> Encyclopedia a -> Bool
layered'''' a (Leaf' _ v) = maybe True (v >) a
layered'''' a (Node' _ v es) = maybe True (v >) a && all childrenLayered es
  where
    childrenLayered = layered'''' (Just (maximum (layer es)))

layered''''' :: (Ord a) => Maybe a -> Encyclopedia a -> Bool
layered''''' a (Leaf' _ v) = maybe True (v >) a
layered''''' a (Node' _ v es ) = (maybe True (v >) a && cond) && all next es
  where
    maxlayer = maximum (layer es) -- will throw on node with empty list
    minGrandchildren = listToMaybe (concatMap children es)
    cond = maybe True (maxlayer <) minGrandchildren 
    next = layered''''' (Just v)
-- layered''''' a e = maybe True (head (layer [e])>) a 

-- layered'''' ((Node' _ v es),a) = all (> v) (layer es)

layeredNt = layered''''' Nothing (Node' "plonk" 4 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 9], Leaf' "bingo" 5])

layeredNt' :: Bool
layeredNt' = layered''''' Nothing (Node' "plonk" 1 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 4], Leaf' "bingo" 5])

layertest = layered''''' Nothing (Node' "a" 1 [Node' "b" 2 [Leaf' "c" 3], Leaf' "d" 4])

class InVector a where
  (&&&) :: a -> a -> a
  (***) :: a -> a -> Int

instance InVector Bool where
  (&&&) :: Bool -> Bool -> Bool
  (&&&) = (||)
  (***) :: Bool -> Bool -> Int
  (***) l r = if l && r then 1 else 0
