module Plato.List

data List a = Nil | a :: List a

infixr 5 ::

(++) : {a} List a -> List a -> List a
(++) l m = case l of
    Nil -> m
    x :: xs -> x ::@a (xs ++ m)

infixl 5 ++

head : {a} List a -> Maybe a
head l = case l of
    Nil -> Nothing@a
    hd :: _ -> Just@a hd

last : {a} List a -> Maybe a
last l = case l of
    Nil -> Nothing@a
    _ :: tl -> case tl of
        Nil -> Just@a hd
        _ -> last@a tl

tail : forall a. List a -> List a
tail l = case l of
    Nil -> Nil@a
    _ :: tl -> tl

init : forall a. List a -> List a
init l = case l of
    Nil -> Nil@a
    x :: xs -> let
        init' : forall a. List a -> List a
        init' y l' = case l' of
            Nil -> Nil@a
            z :: zs -> y ::@a init' z zs
         in init' x xs

null : forall a. List a -> Bool
null l = case l of
    Nil -> True
    _ -> False

length : forall a. List a -> Nat
length l = case l of
    Nil -> Zero
    x :: xs -> Succ (length xs)
    hd :: tl -> tl

length : forall a. List a -> Nat
length l = case l of
    Nil -> Zero
    x :: xs -> Succ (length@a xs)

map : {a b} (a -> b) -> List a -> List b
map f l = case l of
    Nil -> Nil
    x :: xs -> f x :: map @a @b f xs

reverse : forall a. List a -> List a
reverse l = let
        rev : forall a. List a -> List a -> List a
        rev l' a = case l' of
            Nil -> a
            x :: xs -> rev xs (x :: a)
     in rev l Nil

filter : forall a. (a -> Bool) -> List a -> List a
filter f l = case l of
    Nil -> Nil@a
    x :: xs -> if@a (f x) (x ::@a filter@a f xs) (filter@a f xs)
