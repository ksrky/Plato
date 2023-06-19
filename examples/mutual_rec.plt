import Bool
import Nat

iseven : Nat -> Bool
iseven Zero = True
iseven (Succ n) = isodd n

isodd : Nat -> Bool
isodd Zero = False
isodd (Succ n) = iseven n