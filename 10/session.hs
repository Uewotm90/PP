data Onion a = Core a | Layer (Onion a) deriving (Show)

instance Functor Onion where
  fmap :: (a -> b) -> Onion a -> Onion b
  fmap g (Layer l) = fmap g l
  fmap g (Core v) = Core (g v)

idLaw = (pure id <*> Just 1) == Just 1

-- appLaw = pure (id . (+1)) Just 1 == pure (id) <*> pure(+1) <*> Just 1

-- appLaw :: Bool
-- appLaw = pure::Maybe Int ((id)  1) == ((pure (id)) <*> pure 1)

data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where
  fmap :: (a -> b) -> UTree a -> UTree b
  fmap g (Node e t) = Node (g e) (map (fmap g) t)

boege = Node 1 [Node 2 [Node 3 []],Node 4 [Node 5[]]]

-- letter exercises

-- data Exp a where
--   Var :: a -> Exp a
--   Val :: Integer -> Exp a
--   Add :: (Exp a) -> (Exp a) -> Exp a
--   Mult :: (Exp a) -> (Exp a) -> Exp a
--   deriving Show

data Exp a = Var a | Val Integer | Add ( Exp a ) ( Exp a ) | Mult ( Exp a ) ( Exp a ) deriving Show

instance Functor Exp where  
  fmap :: (a -> b) -> Exp a -> Exp b
  fmap g (Var v)  = Var (g v)
  fmap g (Val v) = Val v
  fmap g (Add l r) = Add (fmap g l) (fmap g r)
  fmap g (Mult l r) = Add (fmap g l) (fmap g r)

-- why is it useful to have Exp be Functor? Re: 