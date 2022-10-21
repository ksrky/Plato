-- | function arguments

g : {a b} (a -> b) -> a -> b
g f x = f x