-- variable declaration

anything : {a} a

data Bool = True | False

not : Bool -> Bool
not b = case b of
        True -> anything
        False -> anything

not True