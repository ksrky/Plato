import Plato.Bool

(^) : Bool -> Bool -> Bool
x ^ y =  (x && not y) || (not x && y)

True ^ False

False ^ False