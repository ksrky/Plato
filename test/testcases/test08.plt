data Nat where
    Zero : Nat
    Succ : Nat -> Nat

infixl 6 +
infixl 7 *


(+) : Nat -> Nat -> Nat
Zero + n = n
Succ m' + n = Succ (m' + n)

(*) : Nat -> Nat -> Nat
Zero * n = Zero
Succ m' * n = n + m' * n

Succ (Succ Zero) * Succ Zero + Succ (Succ (Succ Zero))