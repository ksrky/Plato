-- https://softwarefoundations.cis.upenn.edu/vfa-current/SearchTree.html

import Nat
import Pair

data Tree v where
    E : Tree v
    T : Tree v -> Nat -> v -> Tree v -> Tree v

empty_tree : {v} Tree v
empty_tree = E

bound : {v} Nat -> Tree v -> Bool
bound x E = False
bound x (T l y v r) = case (x < y) :,: (x > y) of
    True :,: _ -> bound x l
    False :,: True -> bound x r
    False :,: False -> True

lookup : {v} v -> Nat -> Tree v -> v
lookup d x t E = d
lookup d x t (T l y v r) = case (x < y) :,: (x > y) of
    True :,: _ -> lookup d x l
    False :,:True -> lookup d x r
    False :,: False -> v

insert : {v} Nat -> v -> Tree v -> Tree v
insert x v t E = T E x v E
insert x v t (T l y v' r) = case (x < y) :,: (x > y) of
    True :,: _ -> T (insert x v l) y v' r
    False :,:True -> T l y v' (insert x v r)
    False :,: False -> T l x v r