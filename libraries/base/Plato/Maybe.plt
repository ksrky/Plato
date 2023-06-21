import Bool

data Maybe a where
    Nothing : Maybe a
    Just : a -> Maybe a

maybe : {a b} b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe n f (Just x) = f x

isJust : {a} Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing : {a} Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromMaybe : {a} a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just v) = v