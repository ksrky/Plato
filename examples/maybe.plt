data Maybe a = Nothing | Just a;

fmap : forall a b. (a -> b) -> Maybe a -> Maybe b;
fmap = \f mx -> case mx of {
    Just x -> Just (f x);
    Nothing -> Nothing;
};

id : forall a. a -> a;
id = \x -> x;

main : Maybe Float;
main = fmap id (Just 3);