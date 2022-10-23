import Data.Bool
import Data.Nat

iseven : Nat -> Bool
iseven n = case n of
    Zero -> True
    Succ n' -> isodd n'

isodd : Nat -> Bool
isodd n = case n of
    Zero -> False
    Succ n' -> iseven n'

main : Bool
main = iseven (Succ (Succ (Succ (Succ Zero))))