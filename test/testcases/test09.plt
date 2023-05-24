data List a where
    Nil : List a
    (::) : a -> List a -> List a

infixr 5 ::

data T = T1 | T2 | T3

case T1 :: T2 :: Nil of
    x :: xs -> x
    _ -> T3