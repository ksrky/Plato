data Nat = Zero | Succ Nat

data Bool = True | False

data List a = Nil | a :: List a

infixr 5 ::

(++) : {a} List a -> List a -> List a
(++) l m = case l of
    Nil -> m
    x :: xs -> x :: (xs ++ m)

infixr 5 ++

filter : {a} (a -> Bool) -> List a -> List a
filter f l = case l of
    Nil -> Nil
    x :: xs -> case f x of
        True -> x :: filter f xs
        False -> filter f xs

(>=) : Nat -> Nat -> Bool
(>=) m n = case n of
    Succ n' -> case m of
        Succ m' -> m' >= n'
        Zero -> False
    Zero -> True

(<) : Nat -> Nat -> Bool
(<) m n = case n of
    Succ n' -> case m of
        Succ m' -> m' < n'
        Zero -> True
    Zero -> False

qsort : List Nat -> List Nat
qsort l = case l of
    Nil -> Nil
    x :: xs ->
        let smaller : List Nat
            smaller = filter (x >=) xs
            larger : List Nat
            larger = filter (x <) xs
         in qsort smaller ++ x :: qsort larger

qsort (Succ (Succ (Succ Zero)) :: Succ (Succ (Succ (Succ Zero))) :: Succ (Succ Zero) :: Succ Zero :: Nil)