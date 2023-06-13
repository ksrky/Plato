data Applicative f where
    Applicative : ({a} a -> f a) 
               -> ({a b} f (a -> b) -> f a -> f b)
               -> Applicative f

pure : {f} Applicative f -> {a} a -> f a
pure (Applicative a _) = a

ap : {f} Applicative f -> {a b} f (a -> b) -> f a -> f b
ap (Applicative _ f) = f