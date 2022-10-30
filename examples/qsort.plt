import Plato.List
import Plato.Nat

qsort : List Nat -> List Nat
qsort l = case l of
    Nil -> Nil
    x :: xs ->
        let smaller = filter (x >=) xs
            larger = filter (x <) xs
         in smaller ++ x :: larger

qsort (three :: four :: two :: one :: Nil)
