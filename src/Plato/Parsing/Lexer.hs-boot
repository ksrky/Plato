module Plato.Parsing.Lexer where

import Plato.Parsing.Monad
import Plato.Parsing.Token
import Plato.Common.Location

code :: Int
layout :: Int
comment :: Int
alexMonadScan :: Parser (Located Token)