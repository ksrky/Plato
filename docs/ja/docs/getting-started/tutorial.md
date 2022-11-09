# チュートリアル

Plato は標準で、libs/base 配下の標準ライブラリを読み込む。
このライブラリを使用したプログラム例を以下に示す。

## フィボナッチ数列（examples/fibonacci.plt）

```haskell
import Plato.Nat

fib : Nat -> Nat
fib n = case n of
    Zero -> Zero
    Succ n' -> case n' of
        Zero -> Succ Zero
        Succ n'' -> fib n'' + fib n'

fib (Succ (Succ (Succ (Succ Zero))))
```

1 行目で fib 関数の型を注釈し、2 行目以降、fib 関数の項を記述する。
Plato では、Haskell のようなレイアウト規則を持たないため、ブレースや行末のセミコロンを記述する必要がある。
main 関数はプログラムのエントリポイントであり、計算結果を文字列に変換してターミナルに出力する。

プログラムは`plato run <file name>`で実行できる。

```command
$ plato run examples/fibonacci.plt
Succ (Succ (Succ Zero))
```
