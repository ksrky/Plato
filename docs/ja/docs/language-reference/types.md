# 型

```
type        : '{' tyvar tyvars '}' type
            | btype '->' type
            | btype

btype       : btype atype
            | atype

atype       : '(' type ')'
            | conid
            | varid
```

## ポリモーフィズム

$$
\begin{align*}
\sigma_{i+1} &= \sigma_i\,|\,\sigma_i \rightarrow \sigma_{i+1} \\
\sigma_0 &= \tau
\end{align*}
$$

## データ型　

```haskell
data Bool = True | False
```

### 一般形

```
datatype : 'data' conid
         | 'data' conid '=' constrs

constrs : constr '|' constrs
        | constr

constr : conid types
```
