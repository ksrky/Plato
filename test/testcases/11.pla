data Nat where
    Zero : Nat
    Succ : Nat -> Nat

(+) : Nat -> Nat -> Nat
Zero + n = n
Succ m + n = Succ (m + n)

infixl 6 +

fib : Nat -> Nat
fib Zero = Zero
fib (Succ Zero) = Succ Zero
fib (Succ (Succ n)) = fib n + fib (Succ n)