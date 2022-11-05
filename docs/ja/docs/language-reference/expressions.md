# 式

```
expr        : lexpr op expr
            | lexpr

lexpr       : '\\' var vars '->' expr
            | 'let' '{' decls '}' 'in' expr
            | 'case' expr 'of' '{' alts '}'
            | fexpr

fexpr       : fexpr aexpr
            | aexpr

aexpr       : '(' expr ')'
            | '(' lexpr op ')'
            | varid
            | conid

```

## 関数適用

## let 式

## case 式

```
alts        : alt ';' alts
            | alt
            | {- empty -}

alt         : pat '->' expr

pat         : lpat consym pat
            | lpat

lpat	    : conid apats
            | apat

apats       : apat apats
            | apat

apat        : '(' pat ')'
            | conid
            | varid
            | '_'
```

## ラムダ式

## 中置式
