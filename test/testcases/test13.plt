-- variable declaration

anything : {a} a

data Bool where
    True : Bool
    False : Bool

not : Bool -> Bool
not True = anything
not False = anything