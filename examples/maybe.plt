data Maybe a = Nothing | Just a;

fmap : forall a b. (a -> b) -> Maybe a -> Maybe b;
fmap = \X Y f mx -> case mx of {
    Just x -> Just [Y] (f x);
    Nothing -> Nothing [Y];
};

id : forall a. a -> a;
id = \X -> \x -> x;

main : Maybe Float;
main = (fmap [Float] [Float]) (id [Float]) (Just [Float] 3);