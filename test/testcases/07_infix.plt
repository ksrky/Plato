-- infix expression

data Nat = Zero | Succ Nat;

infixl 6 +;
infixl 7 *;

(+) : Nat -> Nat -> Nat;
(+) m n = case m of {
    Zero -> n;
    Succ m' -> Succ (m' + n);
};

(*) : Nat -> Nat -> Nat;
(*) m n = case m of {
    Zero -> Zero;
    Succ m' -> n + m' * n;  
};

main : Nat;
main = Succ Zero + Succ (Succ (Succ Zero)) * Succ (Succ Zero);