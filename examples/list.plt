data List a = Nil | Cons a (List a);

data Maybe a = Nothing | Just a;

head : forall a. List a -> Maybe a;
head = \l -> case l of {
    Nil -> Nothing;
    Cons hd tl -> Just hd;
};

main : Float;
main = head (Cons 1 (Cons 2 Nil));