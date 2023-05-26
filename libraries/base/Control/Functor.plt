module Control.Functor

data Functor f = Functor ({a b} (a -> b) -> f a -> f b)

fmap : {f} Functor f -> {a b} (a -> b) -> f a -> f b
fmap f = case f of Functor f' -> f'