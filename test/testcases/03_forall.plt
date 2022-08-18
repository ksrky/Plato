id : forall a. a -> a;
id = \x -> x;

data P;
p : P;

main : P;
main = id [P] p;