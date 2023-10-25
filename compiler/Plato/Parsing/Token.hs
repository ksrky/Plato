module Plato.Parsing.Token where

import Data.Text     qualified as T
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

data Keyword = KwCase | KwData | KwImport | KwInfix | KwInfixL | KwInfixR | KwIn | KwOf | KwLet | KwWhere
    deriving (Eq, Show)

data Symbol = SymArrow | SymBackslash | SymColon | SymDash | SymEqual | SymLBrace | SymLParen | SymRBrace | SymRParen | SymSemicolon | SymUScore | SymVLBrace | SymVRBrace
    deriving (Eq, Show)

commonSymbols :: [(T.Text, Symbol)]
commonSymbols =
    [ ("->", SymArrow)
    , ("\\", SymBackslash)
    , (":", SymColon)
    , ("=", SymEqual)
    ]
instance Pretty Token where
    pretty (TokKeyword k) = pretty k
    pretty (TokSymbol t)  = pretty t
    pretty (TokVarId t)   = pretty t
    pretty (TokConId t)   = pretty t
    pretty (TokVarSym t)  = pretty t
    pretty (TokConSym t)  = pretty t
    pretty (TokDigit d)   = pretty d
    pretty TokEOF         = "<eof>"

instance Pretty Keyword where
    pretty KwCase   = "case"
    pretty KwData   = "data"
    pretty KwImport = "import"
    pretty KwInfix  = "infix"
    pretty KwInfixL = "infixl"
    pretty KwInfixR = "infixr"
    pretty KwIn     = "in"
    pretty KwOf     = "of"
    pretty KwLet    = "let"
    pretty KwWhere  = "where"

instance Pretty Symbol where
    pretty SymArrow     = "->"
    pretty SymBackslash = "\\"
    pretty SymColon     = ":"
    pretty SymDash      = "'"
    pretty SymEqual     = "="
    pretty SymLBrace    = "{"
    pretty SymLParen    = "("
    pretty SymRBrace    = "{"
    pretty SymRParen    = ")"
    pretty SymSemicolon = ";"
    pretty SymUScore    = "_"
    pretty SymVLBrace   = "v{"
    pretty SymVRBrace   = "v}"
