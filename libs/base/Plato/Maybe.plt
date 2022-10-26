module Plato.Maybe

import Plato.Bool

data Maybe a = Nothing | Just a

maybe : {a b} b -> (a -> b) -> Maybe a -> b
maybe n f mb = case mb of
        Nothing -> n
        Just x -> f x

isJust : {a} Maybe a -> Bool
isJust mb = case mb of
        Nothing -> False
        _ -> True

isNothing : {a} Maybe a -> Bool
isNothing mb = case mb of
        Nothing -> True
        _ -> False

fromMaybe : {a} a -> Maybe a -> a
fromMaybe d x = case x of
        Nothing -> d
        Just v  -> v