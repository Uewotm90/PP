-- quango :: a -> [ a ]

-- tango :: Num p1 => ( a , b ) -> p2 -> p1

-- 1. opgave

{- 
    a function that takes a value of type a and produces a value of same type.
    second parameter is a value of type a. 
    the entire function returns a value of type a 
-}
-- twice :: (a -> a) -> a -> a 
-- twice f x = f ( f ( x ) ) 

-- twice :: a -> b -> b 

-- 1.2

{-
    twicetwo takes a pair
        first element is a function taking a value of type a and returning a value of type a
        second element is a value of type a
    The function returns a value of type a
-}


-- twicetwo :: ((a -> a),a) -> a
-- twicetwo (f,x) = f (f (x))


-- 3. opgave

{- 
    takes a tuple (pair) of values of type a
    returns a list of a's
 -}

-- dingo :: (a,a) -> [a]
-- dingo ( x , y ) = [ x , y ]


-- 4. opgave

{- 
    functions cannot be Eq, since it is (generally) impossible to test if a pair of functions produce the same values for every input
-}

-- a. opgave

-- mango :: Num a => a -> a -> a -> a
-- mango x y z = x * y + z - 42
-- mango 14 :: Int -> Int -> Int

-- b. opgave

-- bingo :: a -> a

-- c. opgave

-- thesame :: Eq a => [(a,a)] -> [(a,a)]


-- d. opgave

-- aka. a list of binary infix functions (operators)
-- [ (+) , (*) , (+) , (-) ] :: [(a -> a -> a)]
-- [ (+) , (*) , (+) , (-), (++) ] :: Exception (virker åbenbart)

-- e. opgave

-- mymap :: (a -> b) -> [a] -> [b] 
-- mymap f xs = 

-- f. opgave
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

somefunc :: (Ord a1, Eq a2) =>a2 -> a2 -> (a1, a1)-> a1
somefunc a b (x,y) = if a == b then x else y

-- g. opgave

--madras (f,x,y) = f (f x x) y

-- madras' :: (t -> t -> t) -> t -> t -> t
madras' f x y = f (f x x) y
