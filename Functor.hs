import Data.Char

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show


instance Functor (Entry k1 k2) where
    fmap f (Entry (a,b) c) = Entry (a,b) (f c)

instance Functor (Map k1 k2) where
    fmap g (Map []) = Map []
    fmap g (Map a) = Map (map (fmap g) a)

--fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
