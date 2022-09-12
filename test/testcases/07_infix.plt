-- infix expression

data Nat = Zero | Succ Nat


(+) : Nat -> Nat -> Nat;
m + n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

infixl 6 +

(*) : Nat -> Nat -> Nat;
m * n = case m of
    Zero -> Zero
    Succ m' -> n + m' * n

infixl 7 *

main : Nat
main = Succ Zero + Succ (Succ (Succ Zero)) * Succ (Succ Zero)
