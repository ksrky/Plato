module Plato.Either;

import Plato.Bool;

data Either a b = Left a | Right b;

either : forall a b c. (a -> c) -> (b -> c) -> Either a b -> c;
either f g e = case e of {
    Left x -> f x;
    Right y -> g y;
};

isLeft : forall a b. Either a b -> Bool;
isLeft e = case e of {
    Left x -> True;
    _ -> False;
};

isRight : forall a b. Either a b -> Bool;
isRight e = case e of {
    Right y -> True;
    _ -> False;
};
