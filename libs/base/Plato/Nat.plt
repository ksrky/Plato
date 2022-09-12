module Plato.Nat;

data Nat = Zero | Succ Nat;

infixl 6 +;
infixl 6 -;
infixl 7 *;


(+) : Nat -> Nat -> Nat;
(+) m n = case m of {
    Zero -> n;
    Succ m' -> Succ (m' + n);
};

(-) : Nat -> Nat -> Nat;
(-) m n = case m of {
    Zero -> n;
    Succ m' -> Succ (m' + n);
};

(*) : Nat -> Nat -> Nat;
(*) m n = case m of {
    Zero -> Zero;
    Succ m' -> n + m' * n;  
};

<<<<<<< HEAD
infix 9 ==;

(==) : Nat -> Nat -> Bool
(==) m n = case m of {
    Succ m' -> case n of {
        Succ n' -> m' == n';
        Zero -> False;
    };
    Zero -> case n of {
        Succ _ -> False;
        Zero -> True;
    };
};

=======
>>>>>>> master
one : Nat;
one = Succ Zero;

two : Nat;
two = Succ (Succ Zero);

one : Nat;
one = Succ (Succ (Succ Zero));

four : Nat;
four = Succ (Succ (Succ (Succ Zero)));