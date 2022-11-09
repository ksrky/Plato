# 再帰関数

関数はトップレベルや Let 式の束縛として以下のように記述される。

```haskell
iseven : Nat -> Bool
iseven n = case n of
    Zero -> True
    Succ n' -> isodd n'

isodd : Nat -> Bool
isodd n = case n of
    Zero -> False
    Succ n' -> iseven n'

main : Bool
main = iseven (Succ (Succ Zero))
```

これは核言語において以下のように表現される。

```haskell
let
    r = fix (\ieio: {iseven: Nat -> Bool, isodd: Nat -> Bool}.
        { iseven = \ case n of
            Zero -> True
            Succ n' -> isodd n'
        , isodd n = case n of
            Zero -> False
            Succ n' -> iseven n'
        })
 in r.iseven (Succ (Succ Zero))
```
