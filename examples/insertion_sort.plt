-- https://softwarefoundations.cis.upenn.edu/vfa-current/Sort.html

import Plato.Bool
import Plato.Nat
import Plato.List

insert : Nat -> List Nat -> List Nat
insert i l = case l of
    Nil -> i :: Nil
	h :: t -> case i <= h of
		True -> i :: h :: t
		False -> h :: insert i t

sort : List Nat -> List Nat
sort l = case l of
	Nil -> Nil
	h :: t -> insert h (sort t)


-- abbreviation
s : Nat -> Nat
s = Succ

z : Nat
z = Zero

pi_list : List Nat
pi_list = s s s z :: s z :: s s s s z :: s z :: s s s s s z
	:: s s s s s s s s s z :: s s z :: s s s s s s z :: s s s s s z
	:: s s s z :: s s s s s z
---------------

sort pi_list