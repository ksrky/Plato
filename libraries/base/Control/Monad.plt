data Monad m where
    Monad : ({a} a -> m a)
         -> ({a b} m a -> (a -> m b) -> m b)
         -> Monad m

monadReturn : {m} Monad m -> {a} a -> m a
monadReturn (Monad r _) = r

monadBind : {m} Monad m -> {a b} m a -> (a -> m b) -> m b
monadBind (Monad _ b) = b