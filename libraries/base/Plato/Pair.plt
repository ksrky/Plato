data Pair a b = Pair a b

fst : {a b} Pair a b -> a
fst (Pair x _) = x

snd : {a b} Pair a b -> b
snd (Pair _ y) = y

curry : {a b c} (Pair a b -> c) -> a -> b -> c
curry f x y = f (Pair x y)

uncurry : {a b c} (a -> b -> c) -> Pair a b -> c
uncurry (Pair x y) p = f x y

swap : {a b} Pair a b -> Pair b a
swap (Pair x y) = Pair y x