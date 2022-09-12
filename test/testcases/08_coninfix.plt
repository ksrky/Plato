data List a = Nil | a :: List a

infixr 5 ::

data Maybe a = Nothing | Just a

head : {a} -> List a -> Maybe a
head = \l -> case l of
    Nil -> Nothing
    hd :: tl -> Just hd

data T = T1 | T2

main : T
main = head (T1 :: T2)