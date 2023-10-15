# Plato

[![Haskell CI](https://github.com/ksrky/Plato/actions/workflows/haskell.yml/badge.svg)](https://github.com/ksrky/Plato/actions/workflows/haskell.yml)
[![Plato Github Pages](https://github.com/ksrky/Plato/actions/workflows/docs-gh-pages.yml/badge.svg)](https://github.com/ksrky/Plato/actions/workflows/docs-gh-pages.yml)

Plato is a purely functional programming language based on higher-order polymorphic lambda calculus. Only an interpreter is currently implemented that evaluates expressions on the core language.

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

### Prerequisites

Source files are written in GHC2021, so at least ghc-9.2 is required.

- `ghc` >= 9.2
- `stack` or `cabal`

### Building and Installing

If you are using GHC version 9.4.7, you can install the package using Stack:
```
$ stack install
```
If you are not using this GHC version, create your own stack.yaml that aligns with your environment.

`cabal install` can be another option, but make sure that ghc version used by cabal matches the prerequisite.

```command
$ cabal update
$ cabal install
```

If you can run the following command, Plato is successfully installed.

```
$ plato --version
Plato version 1.0.3
```

## Getting started

Since Plato does not have an entry point in a file, it is necessary to display outputs in combination with an interactive shell. For example, if you want to evaluate an expression using the functions declared in `A.pla` and `B.pla`, you can execute it as follows.

```
$ plato A.pla B.pla
>>
```

Note that if B.pla depends on A.pla, these files must be written in the same order as the above command.

Let us run some files under [examples](examples) directory.

```
$ cd path/to/Plato
$ plato examples/fibonacci.pla -i libraries/base
>> fib (S (S (S Z)))
(`S, fold (`S, fold (`Z, `unit)))
>> :q
$ plato examples/quick_sort.pla -i libraries/base
>> qsort (S (S Z) :: Z :: S Z :: S (S (S Z)) :: Nil)
( `::
, fold ( (`Z, `unit)
       , ( `::
         , fold ( (`S, fold (`Z, `unit))
                , ( `::
                  , fold ( (`S, fold (`S, fold (`Z, `unit)))
                         , ( `::
                           , fold ( (`S, fold (`S, fold (`S, fold (`Z, `unit))))
                                  , (`Nil, `unit) ) ) ) ) ) ) ) )
```

`-i` option appends a colon-separated list of dirs to the search path. All the files under the search path are compiled before compiling the main files.

Type `plato --help` to check more commands and options.
