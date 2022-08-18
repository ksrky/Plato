data List a = Nil | Cons a (List a);

data Maybe a = Nothing | Just a;

head : forall a. List a -> Maybe a;
head = \X l -> case l of {
    Nil -> Nothing [X];
    Cons hd tl -> Just [X] hd;
};

data T = T1 | T2;

main : T;
main = head [T] (Cons [T] T1 (Cons [T] T2 (Nil [T])));
