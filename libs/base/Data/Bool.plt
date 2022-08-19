module Data.Bool;

data Bool = True | False;

not : Bool -> Bool;
not = \b -> case b of {
    True -> False;
    False -> True;
};
