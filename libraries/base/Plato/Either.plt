import Bool

data Either a b where
    Left : a -> Either a b
    Right : b -> Either a b

either : {a b c} (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

isLeft : {a b} Either a b -> Bool
isLeft (Left x) = True
isLeft _ = False

isRight : {a b} Either a b -> Bool
isRight (Right y) = True
isRight _ = False