data Nat = Zero | Succ Nat

(+) : Nat -> Nat -> Nat
(+) m n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

infixl 6 +

fib : Nat -> Nat
fib n = case n of
    Zero -> Zero
    Succ n' -> case n' of
        Zero -> Succ Zero
        Succ n'' -> fib n'' + fib n'

fib (Succ (Succ (Succ (Succ Zero))))