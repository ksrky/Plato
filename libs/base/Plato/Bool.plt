module Plato.Bool

data Bool = True | False

not : Bool -> Bool
not b = case b of
    True -> False
    False -> True

if : {a} Bool -> a -> a -> a
if test then else = case test of
    True -> then
    False -> else
