infixr 5 ::;

data List a = Nil | (::) a (List a);

data Maybe a = Nothing | Just a;

head : forall a. List a -> Maybe a;
head = \l -> case l of {
    Nil -> Nothing@a;
    hd :: tl -> Just@a hd;
};

data T = T1 | T2;

main : T;
<<<<<<< HEAD:test/testcases/08_conifix.plt
main = head (T1 ::@T T2);
=======
main = head@T (T1 ::@T T2);
>>>>>>> master:test/testcases/08_coninfix.plt
