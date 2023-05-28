data List a where
    Nil : List a
    (::) : a -> List a -> List a

infixr 5 ::

(++) : {a} List a -> List a -> List a
Nil ++ m = m
(x :: xs) ++ m = x :: (xs ++ m)

infixr 5 ++