-- | case expression

data Bool where
    True : Bool
    False : Bool

not : Bool -> Bool
not b = case b of
    True -> False
    False -> True