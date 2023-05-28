data a :,: b where
    (:,:) : a -> b -> Pair a b

fst : {a b} a :,: b -> a
fst (x :,: _) = x

snd : {a b} Pair a b -> b
snd (_ :,: y) = y

curry : {a b c} (a :,: b -> c) -> a -> b -> c
curry f x y = f (x :,: y)

uncurry : {a b c} (a -> b -> c) -> a :,: b -> c
uncurry (x :,: y) p = f x y

swap : {a b} Pair a b -> b :,: a
swap (x :,: y) = y :,: x