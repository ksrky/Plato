import Plato.List
import Plato.Nat

qsort : List Nat -> List Nat
qsort l = case l of
    Nil -> Nil
    x :: xs ->
        let smaller : List Nat
            smaller = filter (x >=) xs
            larger : List Nat
            larger = filter (x <) xs
         in qsort smaller ++ x :: qsort larger

qsort (three :: four :: two :: one :: Nil)
