module Plato.List;

infixr 5 ::;

data List a = Nil | (::) a (List a);

infixl 5 ++;

(++) : forall a. List a -> List a -> List a;
(++) l m = case l of {
    Nil -> m;
    x :: xs -> x ::@a (xs ++ m);
};

head : forall a. List a -> Maybe a;
head l = case l of {
    Nil -> Nothing@a;
    hd :: _ -> Just@a hd;
};

tail : forall a. List a -> List a;
tail l = case l of {
    Nil -> Nil@a;
    hd :: tl -> tl;
};

length : forall a. List a -> Nat;
length l = case l of {
    Nil -> Zero;
    x :: xs -> Succ (length@a xs);
};

map : forall a b. (a -> b) -> List a -> List b;
map f l = case l of {
    Nil -> Nil@b;
    x :: xs -> f x :: map@a@b f xs;
};

filter : forall a. (a -> Bool) -> List a -> List a;
filter f l = case l of {
    Nil -> Nil@a;
    x :: xs -> if@a (f x) (x ::@a filter@a f xs) (filter@a f xs);
};
