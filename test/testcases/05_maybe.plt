-- type operator

data Maybe a = Nothing | Just a;

fmap : forall a b. (a -> b) -> Maybe a -> Maybe b;
fmap = \f mx -> case mx of {
    Just x -> Just [b] (f x);
    Nothing -> Nothing [b];
};

id : forall a. a -> a;
id = \x -> x;

data T = T1 | T2;

main : Maybe T;
main = (fmap [T] [T]) (id [T]) (Just [T] T1);