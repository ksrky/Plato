import Bool

data Nat where
    Zero : Nat
    Succ : Nat -> Nat

infixl 6 +
infixl 6 -
infixl 7 *

(+) : Nat -> Nat -> Nat
Zero + n = n
Succ m + n = Succ (m + n)

(-) : Nat -> Nat -> Nat
Zero - n = Zero
m - Zero = m
Succ m - Succ n = m - n

(*) : Nat -> Nat -> Nat
Zero * n = Zero
Succ m * n = n + m * n  

infix 4 ==
infix 4 <
infix 4 <=
infix 4 >
infix 4 >=

(==) : Nat -> Nat -> Bool
Succ m == Succ n = m == n
Succ _ == Zero = False
Zero == Succ _ = False
Zero == Zero = True

(<) : Nat -> Nat -> Bool
Succ m < Succ n = m < n
Zero < Succ _ = True
_ < Zero = False

(<=) : Nat -> Nat -> Bool
Succ m <= n = m < n
Zero <= n = True

(>) : Nat -> Nat -> Bool
Succ m > Succ n =  m > n
Succ _ > Zero = True
Zero > n = False

(>=) : Nat -> Nat -> Bool
m >= (Succ n) = m > n
m >= Zero = True