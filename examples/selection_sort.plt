import Nat
import List
import Pair

select : Nat -> List Nat -> Nat :,: List Nat 
select x Nil = x :,: Nil
select x (h :: t) = case x <= h of
    True ->
        let j :,: l = select x t
         in j :,: h :: l
    False ->
        let j :,: l = select h t
         in j :,: x :: l

selsort : List Nat -> List Nat
selsort [] = []
selsort (x :: r) =
    let y :,: r = select x r
     in y :: selsort r