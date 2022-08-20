import Data.Nat;

fib : Nat -> Nat;
fib n = case n of {
    Zero -> Zero
    Succ Zero -> Succ Zero;
    n' -> fib (n' - 2) * fib (n' - 1);
}