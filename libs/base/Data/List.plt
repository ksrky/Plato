module Data.List;

import Data.Maybe;
import Data.Nat;

data List a = Nil | Cons a (List a);

head : forall a. List a -> Maybe a;
head = \l -> case l of {
    Nil -> Nothing [a];
    Cons hd tl -> Just [a] hd;
};

last : forall a. List a -> Maybe a;
last = \l -> case l of {
    Nil -> Nothing [a];
    Cons hd tl -> case tl of {
        Nil -> Just [a] hd;
        _ -> last tl;
    };
};

tail : forall a. List a -> List a;
tail l = case l of {
    Nil -> Nil [a];
    Cons hd tl -> tl;
};

init : forall a. List a -> List a;
init l = case l of {
    Nil -> Nil [a];
    Cons x xs -> let {
        init' : forall a. List a -> List a;
        init' y l' = case l' of {
            Nil -> Nil [a];
            Cons z zs -> Cons [a] y (init' z zs);
        } in init' x xs 
    };
};

null : forall a. List a -> Bool;
null l = case l of {
    Nil -> True;
    _ -> False; 
}

length : forall a. List a -> Nat;
length l = case l of {
    Nil -> Zero;
    Cons x xs -> Succ (length [a] xs);
};

map : forall a b. (a -> b) -> [a] -> [b];
map f l = case l of {
    Nil -> Nil [b];
    Cons x xs -> Cons (f x) (map [a, b] f xs);
};

reverse : forall a. List a -> List a;
reverse l = let {
        rev : forall a. List a -> List a -> List a;
        rev l' a = case l' of {
            Nil -> a;
            Cons x xs -> rev xs (Cons x a);
    };
};
