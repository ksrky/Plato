# Data Types

```haskell
data Bool = True | False
```

## General form

```
datatype : 'data' conid
         | 'data' conid '=' constrs

constrs : constr '|' constrs
        | constr

constr : conid types
```
