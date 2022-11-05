# 変数宣言

関数宣言において、関数本体が欠落すると、変数宣言となる。例：

```haskell
anything : {a} a
```

変数は式の中で型検査はできても、評価することはできないため、その文字列が出力される。例：

```haskell
double : Nat -> Nat
double m = anything

double (Succ Zero)
-- anything
```

変数の型がより具体的に分かっているときには、関数本体の記述を保留するような扱い方ができる。

```haskell
plus : Nat -> Nat -> Nat
```

```haskell
double : Nat -> Nat
double m = plus m m
```

変数は Haskell における`undefined`に似ているが、異なる点は、Plato では変数に出会っても評価を停止せず、その名前を返し、また、型をより具体化できる点である。
