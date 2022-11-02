data Nat = Zero | Succ Nat

(+) : Nat -> Nat -> Nat
(+) m n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

infixl 6 +

(Zero +) (Succ Zero)