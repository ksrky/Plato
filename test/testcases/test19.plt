-- type synonym

data List a = Nil | a :: List a

data Nat = Zero | Succ Nat

NatList = List Nat

head : NatList -> Nat
head l = case l of
    Nil -> Zero
    hd :: _ -> hd

head (Zero :: Succ Zero :: Succ (Succ Zero))