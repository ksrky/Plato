-- https://softwarefoundations.cis.upenn.edu/vfa-current/Sort.html

import Bool
import Nat
import List

insert : Nat -> List Nat -> List Nat
insert i Nil = i :: Nil
insert i (h :: t) = case i <= h of
    True -> i :: h :: t
    False -> h :: insert i t

sort : List Nat -> List Nat
sort Nil = Nil
sort (h :: t) = insert h (sort t)