data Functor f where Functor : ({a b} (a -> b) -> f a -> f b) -> Functor f

fmap : {f} Functor f -> {a b} (a -> b) -> f a -> f b
fmap (Functor f) = f