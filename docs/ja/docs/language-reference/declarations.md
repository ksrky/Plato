# 宣言

## 関数宣言

関数宣言は、関数本体と型署名からなる。プログラム中でこれらはどこに配置されてもよいし、前後が入れ替わっていても問題ないが、型署名の記述は必須となる。例：

```haskell
not : Bool -> Bool
not = \b -> case b of
	True -> False
	False -> True
```

式の先頭でラムダ抽象された変数は`=`の左側に書くこともできる。

```haskell
not : Bool -> Bool
not b = case b of
	True -> False
	False -> True
```

入力をパターンで書くことはできないため、関数本体は必ず一本の式になる。

## 変数宣言

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

# 型宣言

型宣言はデータ型宣言のみで、型の別名を宣言することはできない。
データ型は以下のように定義できる。

```haskell
data Bool = True | False
```

`Bool`の定義において、`True`と`False`をデータコンストラクタと呼び、これらは`Bool`型のラベルでもあり、`Bool`型を構築する関数でもある。
例えば、データコンストラクタが case 式のパターンマッチに現れるときは単なるラベルであるが、その右側に現れるのは`True : Bool`または`False : Bool`の型を持つ値である。

```haskell
not : Bool -> Bool
not b = case b of
	True {- label -} -> False {- value -}
	False {- label -} -> True {- value -}
```

データコンストラクタのないデータ型を書くこともできる。

```haskell
data Void
```

`Void`型は値を持たないが、内部的には空のバリアントとして扱われるため、次のような関数が書ける。

```haskell
absurd : {a} Void -> a
absurd x = case x of
```

## 演算子の結合性宣言
Platoではプログラマが自由に演算子を宣言できるため、その演算子の結合性も定義する必要がある。
infix（結合性なし）、infixl（左結合）、infixr（右結合）のうちどれかと、結合の強さを0~9の値から選択する
```
fixdecl     : 'infix' int op
            | 'infixl' int op
            | 'infixr' int op
```
