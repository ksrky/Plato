($) : {a b} (a -> b) -> a -> b
f $ x = f x

infixr 0 $

data Nat = Zero | Succ Nat

s : Nat -> Nat
s = Succ

(+) : Nat -> Nat -> Nat
(+) m n = case m of
    Zero -> n
    Succ m' -> Succ (m' + n)

(*) : Nat -> Nat -> Nat
(*) m n = case m of
    Zero -> Zero
    Succ m' -> n + m' * n  

infixl 6 +
infixl 7 *

fact : Nat -> Nat
fact n = case n of
        Succ n' -> n * fact n'
        Zero -> Succ Zero

-- fact (s $ s Zero) -- 2!
fact (s $ s $ s $ s Zero) -- 4!
-- fact (s $ s $ s $ s $ s $ s Zero) -- 6!
-- fact (s $ s $ s $ s $ s $ s $ s $ s Zero) -- 8!
-- fact (s $ s $ s $ s $ s $ s $ s $ s $ s $ s $ Zero) -- 10!