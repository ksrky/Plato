module Plato.Abstract.Token where

import Plato.Common.Pretty

data Keyword
        = KwCase
        | KwData
        | KwForall
        | KwImport
        | KwIn
        | KwOf
        | KwLet
        | KwModule
        | KwWhere
        deriving (Eq, Show)

data Symbol
        = SymApost
        | SymArrow
        | SymBackslash
        | SymColon
        | SymComma
        | SymDot
        | SymEqual
        | SymLBrace
        | SymLBrack
        | SymLParen
        | SymRBrace
        | SymRBrack
        | SymRParen
        | SymSemicolon
        | SymUScore
        | SymVBar
        deriving (Eq, Show)

instance Pretty Keyword where
        pretty KwCase = "case"
        pretty KwData = "data"
        pretty KwForall = "forall"
        pretty KwImport = "import"
        pretty KwIn = "in"
        pretty KwOf = "of"
        pretty KwLet = "let"
        pretty KwModule = "module"
        pretty KwWhere = "where"

instance Pretty Symbol where
        pretty SymApost = "'"
        pretty SymArrow = "->"
        pretty SymBackslash = "\\"
        pretty SymColon = ":"
        pretty SymComma = ","
        pretty SymDot = "."
        pretty SymEqual = "="
        pretty SymLBrace = "{"
        pretty SymLBrack = "["
        pretty SymLParen = "("
        pretty SymRBrace = "{"
        pretty SymRBrack = "]"
        pretty SymRParen = ")"
        pretty SymSemicolon = ";"
        pretty SymUScore = "_"
        pretty SymVBar = "|"