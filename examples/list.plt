data List a = Nil | Cons a (List a);

data Maybe a = Nothing | Just a;

head : forall a. List a -> Maybe a;
head = \X l -> case l of {
    Nil -> Nothing [X];
    Cons hd tl -> Just [X] hd;
};

main : Float;
main = head (Cons [Float] 1 (Cons [Float] 2 (Nil [Float])));