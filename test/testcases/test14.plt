infixr 5 ::

data List a where
    Nil : List a
    (::) : a -> List a -> List a


data T where
    T1 : T
    T2 : T
    T3 : T

f : List T
f = T1 :: T2 :: T3 :: Nil