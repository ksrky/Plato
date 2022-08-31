data List a = Nil | Cons a (List a);

data Maybe a = Nothing | Just a;

head : forall a. List a -> Maybe a;
head = \l -> case l of {
    Nil -> Nothing@a;
    Cons hd tl -> Just@a hd;
};

data T = T1 | T2;

main : T;
main = head@T (Cons@T T1 (Cons@T T2 (Nil@T)));

reverse : forall a. List a -> List a;
reverse l = let {
        rev : forall b. List b -> List b -> List b;
        rev l' r = case l' of {
            Nil -> r;
            Cons x xs -> rev@a xs (Cons@a x r);
        };
    } in rev@a l (Nil@a);