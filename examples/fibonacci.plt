import Data.Nat;

fib : Nat -> Nat;
fib n = case n of {
    Zero -> Zero
    Succ n' -> case n' of {
        Zero -> Succ Zero;
        Succ n'' -> fib (n'' - 2) * fib (n'' - 1);
    };
};
