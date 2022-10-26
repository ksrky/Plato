module Plato.List

import Plato.Bool
import Plato.Nat

data List a = Nil | a :: List a

infixr 5 ::

(++) : {a} List a -> List a -> List a
(++) l m = case l of
    Nil -> m
    x :: xs -> x :: (xs ++ m)

infixl 5 ++

head : {a} List a -> Maybe a
head l = case l of
    Nil -> Nothing
    hd :: _ -> Just hd

last : {a} List a -> Maybe a
last l = case l of
    Nil -> Nothing
    _ :: tl -> case tl of
        Nil -> Just hd
        _ -> last tl

tail : {a} List a -> List a
tail l = case l of
    Nil -> Nil
    _ :: tl -> tl

init : {a} List a -> List a
init l = case l of
    Nil -> Nil
    x :: xs -> let
        init' : {a} List a -> List a
        init' y l' = case l' of
            Nil -> Nil
            z :: zs -> y :: init' z zs
         in init' x xs

null : {a} List a -> Bool
null l = case l of
    Nil -> True
    _ -> False

length : {a} List a -> Nat
length l = case l of
    Nil -> Zero
    x :: xs -> Succ (length xs)

map : {a b} (a -> b) -> List a -> List b
map f l = case l of
    Nil -> Nil
    x :: xs -> f x :: map f xs

reverse : {a} List a -> List a
reverse l =
    let rev : {a} List a -> List a -> List a
        rev l' a = case l' of
            Nil -> a
            x :: xs -> rev xs (x :: a)
     in rev l Nil

filter : {a} (a -> Bool) -> List a -> List a
filter f l = case l of
    Nil -> Nil
    x :: xs -> if (f x) (x :: filter f xs) (filter f xs)
