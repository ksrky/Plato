-- | Function clauses

data Bool where
    True : Bool
    False : Bool

not : Bool -> Bool
not True = False
not False = True
