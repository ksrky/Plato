# Plato

[![Haskell CI](https://github.com/ksrky/Plato/actions/workflows/haskell.yml/badge.svg)](https://github.com/ksrky/Plato/actions/workflows/haskell.yml)

Plato is a purely functional programming language based on higher-order polymorphic lambda calculus.

## Language Features

- Statically typed
- Haskell-like syntax
- N-rank parametric polymorphism
- No literal or builtin constant
- Able to evaluate the core language

## Installation

### Getting the source from GitHub

```command
$ git clone https://github.com/ksrky/Plato.git
```

### Prerequisities

```
ghc >= 9.2.7
stack >= 2.9.3
```

### Building and Installing

```command
$ cd Plato
$ stack install
```

If you can run the following command, Plato is successfully installed.
```
$ plato --version
Plato version 1.0.0
```