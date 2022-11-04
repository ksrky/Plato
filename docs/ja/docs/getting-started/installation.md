# インストール

## ソースの入手

Github から

```command
$ git clone https://github.com/ksrky/Plato.git
```

## Building and Installing

Stack を使ってビルドする。バージョンは 2.7.5 のみでテストされている。<br>
[Stack のインストール方法](https://docs.haskellstack.org/en/stable/README/)

```command
$ cd Plato
$ stack install
```

以下のコマンドでパスが通っているか確認できる。

```commad
$ plato --version
Plato version 0.1.0.0
```
