module Plato.Abstract.Token where

import Plato.Common.Pretty

data Posn = Pn Int Int deriving (Eq, Show)

data Token
        = TokKeyword (Keyword, Posn)
        | TokSymbol (Symbol, Posn)
        | TokVarId (String, Posn)
        | TokConId (String, Posn)
        | TokQConId (String, Posn)
        | TokVarSym (String, Posn)
        | TokConSym (String, Posn)
        | TokInt (Int, Posn)
        | TokString (String, Posn)
        | TokEof
        deriving (Eq, Show)

data Keyword
        = KwCase
        | KwData
        | KwForall
        | KwImport
        | KwInfix
        | KwInfixL
        | KwInfixR
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

instance Pretty Posn where
        pretty (Pn l c) = show l ++ ":" ++ show c
instance Pretty Token where
        pretty (TokKeyword (k, p)) = "'" ++ pretty k ++ "' at " ++ pretty p
        pretty (TokSymbol (s, p)) = "'" ++ pretty s ++ "' at " ++ pretty p
        pretty (TokVarId (s, p)) = "'" ++ s ++ "' at " ++ pretty p
        pretty (TokConId (s, p)) = "'" ++ s ++ "' at " ++ pretty p
        pretty (TokQConId (s, p)) = "'" ++ s ++ "' at " ++ pretty p
        pretty (TokVarSym (s, p)) = "'" ++ s ++ "' at " ++ pretty p
        pretty (TokConSym (s, p)) = "'" ++ s ++ "' at " ++ pretty p
        pretty (TokInt (i, p)) = show i ++ " at " ++ pretty p
        pretty (TokString (s, p)) = "\"" ++ s ++ "\" at " ++ pretty p
        pretty TokEof = "<eof>"

instance Pretty Keyword where
        pretty KwCase = "case"
        pretty KwData = "data"
        pretty KwForall = "forall"
        pretty KwImport = "import"
        pretty KwInfix = "infix"
        pretty KwInfixL = "infixl"
        pretty KwInfixR = "infixr"
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
