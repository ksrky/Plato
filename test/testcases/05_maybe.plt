-- type operator

data Maybe a = Nothing | Just a;

fmap : forall a b. (a -> b) -> Maybe a -> Maybe b;
fmap = \X Y f mx -> case mx of {
    Just x -> Just [Y] (f x);
    Nothing -> Nothing [Y];
};

id : forall a. a -> a;
id = \X -> \x -> x;

data T = T1 | T2;

main : Maybe T;
main = (fmap [T] [T]) (id [T]) (Just [T] T1);