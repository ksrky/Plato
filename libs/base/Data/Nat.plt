module Data.Nat;

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
