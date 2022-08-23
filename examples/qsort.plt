import Data.List;
import Data.Nat;

qsort :  List Nat -> List Nat;
qsort l = case l of {
    Nil -> Nil;
    x :: xs -> let {
        smaller = filter (x >=) xs;
        larger = filter (x <) xs;
    } in smaller ++ x :: larger;
};

main : List Nat;
main = qsort (three :: four :: two :: one :: Nil);
