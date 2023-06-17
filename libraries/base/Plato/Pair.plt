data a :,: b where
    (:,:) : a -> b -> a :,: b

infixr 1 :,:

fst : {a b} a :,: b -> a
fst (x :,: _) = x

snd : {a b} a :,: b -> b
snd (_ :,: y) = y

curry : {a b c} (a :,: b -> c) -> a -> b -> c
curry f x y = f (x :,: y)

uncurry : {a b c} (a -> b -> c) -> a :,: b -> c
uncurry f (x :,: y) = f x y

swap : {a b} a :,: b -> b :,: a
swap (x :,: y) = y :,: x