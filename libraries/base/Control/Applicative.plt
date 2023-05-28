module Control.Applicative

data Applicative f = Applicative ({a} a -> f a) ({a b} f (a -> b) -> f a -> f b)

pure : {f} Applicative f -> {a} a -> f a
pure a = case a of Applicative f -> f

ap : {f} Applicative f -> {a b} f (a -> b) -> a -> b
ap a = case a of Applicative f -> f