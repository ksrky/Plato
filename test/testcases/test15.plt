-- Arbitrary rank polymorphism

data ChurchNum where
    ChurchNum : ({a} (a -> a) -> a -> a) -> ChurchNum

runNum : ChurchNum -> ({a} (a -> a) -> a -> a)
runNum (ChurchNum xs) = xs

zero : ChurchNum
zero = ChurchNum (\s z -> z)

succ : ChurchNum -> ChurchNum
succ n = ChurchNum (\s z -> s (runNum n s z))

two : ChurchNum
two = succ (succ zero)