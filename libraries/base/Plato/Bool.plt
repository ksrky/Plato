data Bool where
    True : Bool
    False : Bool

infixr 3 &&
infixr 2 ||

(&&) : Bool -> Bool -> Bool
True && y = y
False && _ = False

(||) : Bool -> Bool -> Bool
True || _  =True
False　|| y -> y

not : Bool -> Bool
not True = False
not False = True

ifThenElse : {a} Bool -> a -> a -> a
ifThenElse True then _ = then
ifThenElse False _ else = else
