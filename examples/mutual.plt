-- mutual recursion

data Nat = Zero | Succ Nat;

data Bool = True | False;

iseven : Nat -> Bool;
iseven = \n -> case n of {
    Zero -> True;
    Succ n' -> isodd n';
};

isodd : Nat -> Bool;
isodd = \n -> case n of {
    Zero -> False;
    Succ n' -> iseven n';
};
