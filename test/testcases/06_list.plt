data List a = Nil | Cons a (List a)

data Maybe a = Nothing | Just a

head : {a} -> List a -> Maybe a
head = \l -> case l of
    Nil -> Nothing
    Cons hd tl -> Just hd

data T = T1 | T2

main : T
main = head (Cons T1 (Cons T2 Nil))
