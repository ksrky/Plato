-- let expression

f : {a} a -> a
f = let g = \x -> x
        h = \y -> y
     in g