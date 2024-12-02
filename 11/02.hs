import Prelude hiding (return, fmap)
data W x = Bingo x deriving Show

instance Functor W where
fmap :: (a -> b) -> W a -> W b
fmap f ( Bingo x ) = Bingo ( f x )

instance Applicative W where
    pure :: a -> W a
    pure x = Bingo (x)
    (<*>) :: W (a -> b) -> W a -> W b
    (Bingo g) <*> r = fmap g r  

instance Monad W where
return x = Bingo x
Bingo x >>= f = f x


wrapadd mx my = do
    x <- mx
    y <- my
    return (Bingo(x+y))