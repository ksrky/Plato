# 関数宣言

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
