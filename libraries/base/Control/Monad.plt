module Control.Monad

data Monad m = Monad ({a} a -> m a) ({a b} m a -> (a -> m b) -> m b)

monadReturn : {m} Monad m -> {a} a -> m a
monadReturn m = case m of Monad r _ -> r

monadBind : {m} Monad m -> {a b} m a -> (a -> m b) -> m b
monadBind m = case m of Monad _ b -> b