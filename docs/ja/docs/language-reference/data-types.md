# Data Types

```haskell
data Bool = True | False
```

## General form
```
datatype : 'data' con
         | 'data' con '=' constrs

constrs : constr '|' constrs
        | constr

constr : con types
```