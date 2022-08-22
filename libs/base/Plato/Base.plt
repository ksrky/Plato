module Plato.Base;

infixr 0 $;

($) :: forall a b. (a -> b) -> a -> b;
($) f x = f x;

infixr . 9;

(.) :: forall a b c. (b -> c) -> (a -> b) -> a -> c;
(.) f g = \x -> f (g x)

id :: forall a. a -> a;
id x = x;

const :: forall a b. a -> b -> a;
const x _ = x;

flip :: forall a b c. (a -> b -> c) -> b -> a -> c;
flip f x y = f y x;
