import List
import Nat

qsort : List Nat -> List Nat
qsort Nil = Nil
qsort (x :: xs) =
    let smaller : List Nat
        smaller = filter ((>=) x) xs
        larger : List Nat
        larger = filter ((<) x) xs
     in qsort smaller ++ x :: qsort larger