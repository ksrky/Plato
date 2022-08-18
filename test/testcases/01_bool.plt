-- data type and case expression

data Bool = True | False;

not : Bool -> Bool;
not = \b -> case b of { True -> False; False -> True; };

main : Bool;
main = not True;