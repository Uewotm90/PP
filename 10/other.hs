{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Prelude hiding (Applicative, Functor, fmap, pure, (<*>))
import Prelude qualified hiding ((<*>))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Functor ((->) r) where
  fmap g h = g . h

new = fmap head tail

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure :: a -> [a]
  pure e = [e]
  (<*>) :: [a -> b] -> [a] -> [b]
  [] <*> xs = []
  (g : gs) <*> xs = (fmap g xs) ++ (gs <*> xs)

prodthree :: [Int] -> [Int] -> [Int] -> [Int]
prodthree f s t = pure (*) <*> (pure (*) <*> f <*> s) <*> t