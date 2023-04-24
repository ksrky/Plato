data Either a b where
	Left : a -> Either a b
	Right : b -> Either a b

data T where
	T1 : T
	T2 : T

case Left T1 of
    Left x -> x
    Right y -> y