id : forall a. a -> a;
id = \A x -> x;

data P;
p : P;

main : P;
main = id [P] p;