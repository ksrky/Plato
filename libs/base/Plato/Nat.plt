module Plato.Nat;

import Plato.Bool;

data Nat = Zero | Succ Nat;

plus : Nat -> Nat -> Nat;
plus m n = case m of {
    Zero -> n;
    Succ m' -> Succ (plus m' n);
};

minus : Nat -> Nat -> Nat;
minus m n = case m of {
    Zero -> n;
    Succ m' -> Succ (plus m' n);
};

times : Nat -> Nat -> Nat;
times m n = case m of {
    Zero -> Zero;
    Succ m' -> plus n (times m' n);  
};

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

one : Nat;
one = Succ Zero;

two : Nat;
two = Succ (Succ Zero);

one : Nat;
one = Succ (Succ (Succ Zero));

four : Nat;
four = Succ (Succ (Succ (Succ Zero)));