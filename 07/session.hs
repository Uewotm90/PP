data Unary = Z | I Unary

one = I Z

two = I one

two' = I (I Z)

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

four = unary2int (I (I (I (I Z))))

data Tree a = Leaf a | Node (Tree a) a (Tree a)

least :: Ord a => Tree a -> a 
least (Leaf x) = x
least (Node l x r) = min x (min (least l) (least r) )

oak = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))