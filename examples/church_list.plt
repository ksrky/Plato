import Plato.Base
import Plato.List

data ChurchList a where
    ChurchList : ({r} (a -> r -> r) -> r -> r) -> ChurchList a

runList : {a} ChurchList a -> ({r} (a -> r -> r) -> r -> r)
runList (ChurchList xs) = xs

fromList : {a} List a -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs

toList : {a} ChurchList a -> List a
toList xs = runList xs (::) Nil

cons : {a} a -> ChurchList a -> ChurchList a
cons x xs = ChurchList $ \k z -> k x (runList xs k z)

append : {a} ChurchList a -> ChurchList a -> ChurchList a
append xs ys = ChurchList $ \k z -> runList xs k (runList ys k z)

nil : {a} ChurchList a
nil = ChurchList $ \k z -> z

singleton : {a} a -> ChurchList a
singleton x = ChurchList $ \k z -> k x z

snoc : {a} ChurchList a -> a -> ChurchList a
snoc xs x = ChurchList $ \k z -> runList xs k (k x z)