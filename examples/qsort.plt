import Data.List;
import Data.Nat;

qsort :  List Nat -> List Nat;
qsort l = case l of {
    Nil -> Nil [Nat];
    Cons x xs -> let {
        smaller = filter [Nat] (<= x) xs;
        larger = filter [Nat] (> x) xs;
    } in smaller ++ Cons x larger;
};

main : List Nat;
main = qsort (Cons three (Cons four (Cons two (Cons one Nil))));
