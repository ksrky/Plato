infixr 5 ::

data List a where
    Nil : List a
    (::) : a -> List a -> List a


reverse : {a} List a -> List a
reverse l =
    let rev : {a} List a -> List a -> List a
        rev Nil a = a
        rev (x :: xs) a = rev xs (x :: a)
     in rev l Nil