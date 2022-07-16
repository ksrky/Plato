data Nat = Zero | Succ Nat;

plus : Nat -> Nat -> Nat;
plus = \m -> \n -> case m of {
    Zero -> n;
    Succ m' -> Succ (plus m' n);
};

main : Nat;
main = plus (Succ (Succ Zero)) (Succ (Succ (Succ Zero)));