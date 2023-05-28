import Plato.Bool

data Nat = Zero | Succ Nat

infixl 6 +
infixl 6 -
infixl 7 *

(+) : Nat -> Nat -> Nat
(+) m n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

(-) : Nat -> Nat -> Nat
(-) m n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

(*) : Nat -> Nat -> Nat
(*) m n = case m of
    Zero -> Zero
    Succ m' -> n + m' * n  

infix 4 ==
infix 4 <
infix 4 <=
infix 4 >
infix 4 >=

(==) : Nat -> Nat -> Bool
(==) m n = case m of
    Succ m' -> case n of
        Succ n' -> m' == n'
        Zero -> False
    Zero -> case n of
        Succ _ -> False
        Zero -> True

(<) : Nat -> Nat -> Bool
(<) m n = case n of
    Succ n' -> case m of
        Succ m' -> m' < n'
        Zero -> True
    Zero -> False

(<=) : Nat -> Nat -> Bool
(<=) m n = case m of
    Succ m' -> m' < n
    Zero -> True

(>) : Nat -> Nat -> Bool
(>) m n = case m of
    Succ m' -> case n of
        Succ n' -> m' > n'
        Zero -> True
    Zero -> False

(>=) : Nat -> Nat -> Bool
(>=) m n = case n of
    Succ n' -> m > n'
    Zero -> True

one : Nat
one = Succ Zero

two : Nat
two = Succ (Succ Zero)

three : Nat
three = Succ (Succ (Succ Zero))

four : Nat
four = Succ (Succ (Succ (Succ Zero)))