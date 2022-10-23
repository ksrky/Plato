module Plato.Either;

data Either a b = Left a | Right b;

either : {a b c} (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of
    Left x -> f x
    Right y -> g y

isLeft : {a b} Either a b -> Bool
isLeft e = case e of
    Left x -> True
    _ -> False

isRight : {a b} Either a b -> Bool
isRight e = case e of
    Right y -> True
    _ -> False
