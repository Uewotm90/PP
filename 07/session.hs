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

data Aexp = Num Int | Var' String | Plus Aexp Aexp | Mult Aexp Aexp

eval' :: Aexp -> (String -> Int) -> Int
eval' (Num n) _ = n
eval' (Var' x) ass = ass x
eval' (Plus l r) ass = eval' l ass + eval' r ass
eval' (Mult l r) ass = eval' l ass * eval' r ass

expr = Plus (Mult (Num 2) (Var' "x")) (Var' "y") -- should evaluate to 10 with the assignments [x->3,y->4]

assignments :: String -> Int
assignments "x" = 3
assignments "y" = 4

evaluated = eval' expr assignments

data Encyclopedia a = Leaf' String a | Node' String a [Encyclopedia a] deriving (Show)

t1 = Node' "mango" True [Node' "dingo" False [Leaf' "plip" True, Leaf' "ninka" False], Leaf' "plop" True, Node' "plys" False [Leaf' "boing" True]]

t2 = Node' "plonk" 1 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 9], Leaf' "bingo" 5]

values :: Encyclopedia a -> [a]
values (Leaf' _ v) = [v]
values (Node' _ v vs) = v : concatMap values vs

layer :: [Encyclopedia a] -> [a]
layer [Leaf' _ v] = [v]
layer [Node' _ v _] = [v]
layer ((Leaf' _ v) : es) = v : layer es
layer ((Node' _ v vs) : es) = v : layer es

children :: Encyclopedia a -> [a]
children (Leaf' _ _) = []
children (Node' _ _ vs) = layer vs

-- sjette gang er lykkens gang :))))
layered''''' :: (Ord a) => Maybe a -> Encyclopedia a -> Bool
layered''''' a (Leaf' _ v) = maybe True (v >) a
layered''''' a (Node' _ v es) = (maybe True (v >) a && cond) && all next es
  where
    maxlayer = maximum (layer es) -- will throw on node with empty list
    minGrandchildren = listToMaybe (concatMap children es)
    cond = maybe True (maxlayer <) minGrandchildren
    next = layered''''' (Just v)

-- should be False
layeredNt = layered''''' Nothing (Node' "plonk" 4 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 9], Leaf' "bingo" 5])

-- should be False
layeredNt' :: Bool
layeredNt' = layered''''' Nothing (Node' "plonk" 1 [Node' "zap" 3 [Leaf' "ninka" 8], Node' "uhu" 4 [Leaf' "gif" 4], Leaf' "bingo" 5])

-- should be False
layertest = layered''''' Nothing (Node' "a" 1 [Node' "b" 2 [Leaf' "c" 3], Leaf' "d" 4])

class InVector a where
  (&&&) :: a -> a -> a
  (***) :: a -> a -> Int

instance InVector Bool where
  (&&&) :: Bool -> Bool -> Bool
  (&&&) = (||)
  (***) :: Bool -> Bool -> Int
  (***) l r = if l && r then 1 else 0

count :: Tree a -> Int
count (Leaf _) = 1
count (Node l _ r) = count l + count r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l _ r) = abs (count l - count r) <= 1 && balanced l && balanced r

unbalanced = balanced (Node (Node (Leaf 3) 2 (Node (Leaf 5) 4 (Leaf 6))) 1 (Leaf 3))

-- from 8.6 in Graham Hutton
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And l r) = eval s l && eval s r
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- not tested :D
equiv :: Prop -> Prop -> Bool
equiv p q = eval [] pq && eval [] qp 
  where
    pq = Imply p q 
    qp = Imply q p 

    