-- type operator

data Maybe a = Nothing | Just a

fmap : {a b} -> (a -> b) -> Maybe a -> Maybe b
fmap f mx = case mx of
    Just x -> Just (f x)
    Nothing -> Nothing

id : forall a. a -> a
id = \x -> x

data T = T1 | T2

main : Maybe T
main = fmap id (Just T1)