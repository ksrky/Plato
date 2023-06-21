import Bool
import Nat
import Maybe

infixr 5 ::
infixr 5 ++

data List a where
    Nil : List a
    (::) : a -> List a -> List a

(++) : {a} List a -> List a -> List a
(++) Nil m = m
(x :: xs) ++ m = x :: (xs ++ m)

head : {a} List a -> Maybe a
head Nil = Nothing
head (hd :: _) = Just hd

last : {a} List a -> Maybe a
last Nil = Nothing
last (hd :: Nil) = Just hd
last (_ :: tl) = last tl

tail : {a} List a -> List a
tail Nil = Nil
tail (_ :: tl) = tl

{-
init : {a} List a -> List a
init Nil = Nil
init (x :: xs) =
    let init' : {a} a -> List a -> List a -- error : Scoped type variables
        init' y Nil = Nil
        init' y (z :: zs) -> y :: init' z zs
     in init' x xs

-- Alternative
init : {a} List a -> List a
init Nil = Nil
init {a} (x :: xs) ->
    let init' : a -> List a -> List a -- error : Scoped type variables
        init' y Nil = Nil
        init' y (z :: zs) -> y :: init' z zs
     in init' x xs
-}

null : {a} List a -> Bool
null Nil = True
null _ = False

length : {a} List a -> Nat
length Nil = Zero
length (x :: xs) = Succ (length xs)

map : {a b} (a -> b) -> List a -> List b
map f Nil = Nil
map f (x :: xs) = f x :: map f xs

reverse : {a} List a -> List a
reverse l =
    let rev : {a} List a -> List a -> List a
        rev l' a = case l' of
            Nil -> a
            x :: xs -> rev xs (x :: a)
     in rev l Nil

filter : {a} (a -> Bool) -> List a -> List a
filter f Nil = Nil
filter f (x :: xs) = ifThenElse (f x) (x :: filter f xs) (filter f xs)

foldr : {a b} (a -> b -> b) -> b -> List a -> b
foldr k z Nil = z
foldr k z (y :: ys) = k y (foldr k z ys)