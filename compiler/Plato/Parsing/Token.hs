module Plato.Parsing.Token where

import Data.Text qualified as T
import Prettyprinter

data Token
        = TokKeyword Keyword
        | TokSymbol Symbol
        | TokVarId T.Text
        | TokConId T.Text
        | TokVarSym T.Text
        | TokConSym T.Text
        | TokDigit Int
        | TokEOF
        deriving (Eq, Show)

data Keyword
        = KwCase
        | KwData
        | KwImport
        | KwInfix
        | KwInfixL
        | KwInfixR
        | KwIn
        | KwOf
        | KwOpen
        | KwLet
        | KwModule
        | KwWhere
        deriving (Eq, Show)

data Symbol
        = SymArrow
        | SymBackslash
        | SymColon
        | SymComma
        | SymDash
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
        | SymVLBrace
        | SymVRBrace
        deriving (Eq, Show)

commonSymbols :: [(T.Text, Symbol)]
commonSymbols =
        [ ("->", SymArrow)
        , ("\\", SymBackslash)
        , (":", SymColon)
        , ("=", SymEqual)
        , ("|", SymVBar)
        ]
instance Pretty Token where
        pretty (TokKeyword k) = pretty k
        pretty (TokSymbol t) = pretty t
        pretty (TokVarId t) = pretty t
        pretty (TokConId t) = pretty t
        pretty (TokVarSym t) = pretty t
        pretty (TokConSym t) = pretty t
        pretty (TokDigit d) = pretty d
        pretty TokEOF = "<eof>"

instance Pretty Keyword where
        pretty KwCase = "case"
        pretty KwData = "data"
        pretty KwImport = "import"
        pretty KwInfix = "infix"
        pretty KwInfixL = "infixl"
        pretty KwInfixR = "infixr"
        pretty KwIn = "in"
        pretty KwOf = "of"
        pretty KwOpen = "open"
        pretty KwLet = "let"
        pretty KwModule = "module"
        pretty KwWhere = "where"

instance Pretty Symbol where
        pretty SymArrow = "->"
        pretty SymBackslash = "\\"
        pretty SymColon = ":"
        pretty SymComma = ","
        pretty SymDash = "'"
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
        pretty SymVLBrace = "v{"
        pretty SymVRBrace = "v}"