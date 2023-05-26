module Plato.Bool

data Bool = True | False

(&&) : Bool -> Bool -> Bool
(&&) x y = case x of
    True -> y
    False -> False

infixr 3 &&

(||) : Bool -> Bool -> Bool
(||) x y = case x of
    True -> True
    False -> y

infixr 2 ||

not : Bool -> Bool
not b = case b of
    True -> False
    False -> True

if : {a} Bool -> a -> a -> a
if test then else = case test of
    True -> then
    False -> else
