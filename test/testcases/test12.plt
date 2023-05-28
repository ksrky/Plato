data Nat where
    Zero : Nat
    Succ : Nat -> Nat

data Bool where
    True : Bool
    False : Bool

(>=) : Nat -> Nat -> Bool
(>=) (Succ m) (Succ n) = m >= n
(>=) Zero (Succ _) = False
(>=) m Zero = True

(<) : Nat -> Nat -> Bool
(<) (Succ m) (Succ n) = m < n
(<) Zero (Succ _) = True
(<) _ Zero = False