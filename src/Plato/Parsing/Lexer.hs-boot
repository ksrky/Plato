module Plato.Parsing.Lexer where

import Plato.Common.SrcLoc
import Plato.Parsing.Monad
import Plato.Parsing.Token

code :: Int
layout :: Int
comment :: Int
alexMonadScan :: Parser (Located Token)
