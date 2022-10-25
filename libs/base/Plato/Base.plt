module Plato.Base

($) : {a b} (a -> b) -> a -> b
f $ x = f x

infixr 0 $

id : {a} a -> a
id x = x

const : {a b} a -> b -> a
const x y = x

flip : {a b c}  (a -> b -> c) -> b -> a -> c
flip f x y = f y x
