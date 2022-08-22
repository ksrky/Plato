module Logic.CH.Propositional;

data And a b = a :/\: b;

data Or a b = Inl a | Inr b;

type Imply a b = a -> b

data Void;

data Not a = a -> Void;

