module Control.Monad

data Monad m = Monad ({a} a -> m a) ({a b} m a -> (a -> m b) -> b)

return :: {m} Monad m -> {a} a -> m a
return m = case m of Monad r _ -> r

bind :: {m} Monad m -> {a b} m a -> (a -> m b) -> b
bind m = case m of Monad _ b -> b