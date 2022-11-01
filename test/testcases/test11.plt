data Either a b = Left a | Right b

data T = T1 | T2

case Left T1 of
        Left x -> x
        Right y -> y