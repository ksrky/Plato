module Pair

data Pair a b = Pair a b

fst : {a b} Pair a b -> a
fst p = case p of Pair x y -> x

snd : {a b} Pair a b -> b
snd p = case p of Pair x y -> y

curry : {a b c} (Pair a b -> c) -> a -> b -> c
curry f x y = f (Pair x y)

uncurry : {a b c} (a -> b -> c) -> Pair a b -> c
uncurry f p = case p of Pair x y -> f x y

swap : {a b} Pair a b -> Pair b a
swap p = case p of Pair x y -> Pair y x