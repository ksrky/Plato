import Plato.Nat

fib : Nat -> Nat
fib n = case n of
    Zero -> Zero
    Succ n' -> case n' of
        Zero -> Succ Zero
        Succ n'' -> fib n'' + fib n'

fib (Succ (Succ (Succ (Succ Zero))))