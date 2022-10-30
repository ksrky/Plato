-- evaluation

data Bool = True | False

(\b -> case b of
    True -> False
    False -> True) False