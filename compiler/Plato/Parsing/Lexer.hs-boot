module Plato.Parsing.Lexer where

import Plato.Common.Location
import Plato.Parsing.Monad
import Plato.Parsing.Token

code :: Int
comment :: Int
layout :: Int
alexMonadScan :: Parser (Located Token)
