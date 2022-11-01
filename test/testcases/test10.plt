data List a = Nil | a :: List a

infixr 5 ::

(++) : {a} List a -> List a -> List a
(++) l m = case l of
    Nil -> m
    x :: xs -> x :: (xs ++ m)

infixr 5 ++

data T = T1 | T2

T1 :: T2 :: Nil ++ T2 :: T1 :: Nil