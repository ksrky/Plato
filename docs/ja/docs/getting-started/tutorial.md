# チュートリアル

Plato は標準で、libs/base 配下の標準ライブラリを読み込む。
このライブラリを使用したプログラム例を以下に示す。

## フィボナッチ数列（examples/fibonacci.plt）

```haskell
fib : Nat -> Nat
fib n = case n of
    Zero -> Zero
    Succ n' -> case n' of
        Zero -> Succ Zero
        Succ n'' -> fib n'' + fib n'

main : Nat
main = fib (Succ (Succ (Succ (Succ Zero))))
```

1 行目で fib 関数の型を注釈し、2 行目以降、fib 関数の項を記述する。
Plato では、Haskell のようなレイアウト規則を持たないため、ブレースや行末のセミコロンを記述する必要がある。
main 関数はプログラムのエントリポイントであり、計算結果を文字列に変換してターミナルに出力する。

プログラムは`plato <file name>`で実行できる。

```command
$ plato examples/fibonacci.plt
Succ (Succ (Succ Zero))
```

デバッグオプションをつけることで、ラムダ項の簡約の様子を追跡できる。

```command
$ plato examples/fibonacci.plt --debug
(let {_0=(fix (λ_0: {fib:(Nat -> Nat)}. {fib=(λn: Nat. (case n of {Zero  -> Zero | Succ 1 -> (case 1 of {Zero  -> (Succ Zero) | Succ 1' -> ((libs/base/Plato/Nat.plt.+ (_0.fib 1')) (_0.fib 1))})}))}))} in (_0.fib (Succ (Succ (Succ (Succ Zero))))))

(let {_0={fib=(λn: Nat. (case n of {Zero  -> Zero | Succ 1 -> (case 1 of {Zero  -> (Succ Zero) | Succ 1' -> ((libs/base/Plato/Nat.plt.+ ((fix (λ_0: {fib:(Nat -> Nat)}. {fib=(λn': Nat. (case n' of {Zero  -> Zero | Succ 1'' -> (case 1'' of {Zero  -> (Succ Zero) | Succ 1''' -> ((libs/base/Plato/Nat.plt.+ (_0.fib 1''')) (_0.fib 1''))})}))})).fib 1')) ((fix (λ_0: {fib:(Nat -> Nat)}. {fib=(λn': Nat. (case n' of {Zero  -> Zero | Succ 1'' -> (case 1'' of {Zero  -> (Succ Zero) | Succ 1''' -> ((libs/base/Plato/Nat.plt.+ (_0.fib 1''')) (_0.fib 1''))})}))})).fib 1))})}))}} in (_0.fib (Succ (Succ (Succ (Succ Zero))))))


(省略)

((λ1: Nat. Succ 1) ((λn: Nat. (case Zero of {Zero  -> n | Succ 1 -> (Succ (((fix (λ_0: {+:(Nat -> (Nat -> Nat)), -:(Nat -> (Nat -> Nat)), *:(Nat -> (Nat -> Nat)), one:Nat, two:Nat, one:Nat, four:Nat}. {+=(λm: Nat. (λn': Nat. (case m of {Zero  -> n' | Succ 1' -> (Succ ((_0.+ 1') n'))}))), -=(λm: Nat. (λn': Nat. (case m of {Zero  -> n' | Succ 1' -> (Succ ((_0.+ 1') n'))}))), *=(λm: Nat. (λn': Nat. (case m of {Zero  -> Zero | Succ 1' -> ((_0.+ n') ((_0.* 1') n'))}))), one=(Succ Zero), two=(Succ (Succ Zero)), one=(Succ (Succ (Succ Zero))), four=(Succ (Succ (Succ (Succ Zero))))})).+ 1) n))})) Succ (Succ Zero)))

((λ1: Nat. Succ 1) (case Zero of {Zero  -> Succ (Succ Zero) | Succ 1 -> (Succ (((fix (λ_0: {+:(Nat -> (Nat -> Nat)), -:(Nat -> (Nat -> Nat)), *:(Nat -> (Nat -> Nat)), one:Nat, two:Nat, one:Nat, four:Nat}. {+=(λm: Nat. (λn: Nat. (case m of {Zero  -> n | Succ 1' -> (Succ ((_0.+ 1') n))}))), -=(λm: Nat. (λn: Nat. (case m of {Zero  -> n | Succ 1' -> (Succ ((_0.+ 1') n))}))), *=(λm: Nat. (λn: Nat. (case m of {Zero  -> Zero | Succ 1' -> ((_0.+ n) ((_0.* 1') n))}))), one=(Succ Zero), two=(Succ (Succ Zero)), one=(Succ (Succ (Succ Zero))), four=(Succ (Succ (Succ (Succ Zero))))})).+ 1) Succ (Succ Zero)))}))

((λ1: Nat. Succ 1) Succ (Succ Zero))

Succ (Succ (Succ Zero))
```
