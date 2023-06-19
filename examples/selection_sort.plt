-- https://softwarefoundations.cis.upenn.edu/vfa-current/Sort.html

import Nat
import List
import Pair

select : Nat -> List Nat -> Nat :,: List Nat 
select x Nil = x :,: Nil
select x (h :: t) = case x <= h of
    True -> case select x t of
        j :,: l -> j :,: h :: l
    False -> case select h t of
        j :,: l -> j :,: x :: l

selsort : List Nat -> List Nat
selsort Nil = Nil
selsort (x :: r) = case select x r of
    y :,: r -> y :: selsort r