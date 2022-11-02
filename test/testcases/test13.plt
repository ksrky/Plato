data List = Nil | a :: List a

reverse : {a} List a -> List a
reverse l =
    let rev : {a} List a -> List a -> List a
        rev l' a = case l' of
            Nil -> a
            x :: xs -> rev xs (x :: a)
     in rev l Nil