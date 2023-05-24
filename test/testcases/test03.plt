-- | recursive data type declaration

data Nat where
    Zero : Nat
    Succ : Nat -> Nat
