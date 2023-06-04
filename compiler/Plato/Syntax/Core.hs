module Plato.Syntax.Core where

import Prettyprinter

import Plato.Common.Name

-- *  Abstract syyntax

data Phrase
        = Prog Prog
        | Term Term
        deriving (Show, Eq)

type Label = Name

data Entry
        = Decl Name Type
        | Defn Name Term
        deriving (Show, Eq)

type Prog = [Entry]

type Type = Term

type Bind a = (Name, a)

data PiSigma
        = Pi
        | Sigma
        deriving (Show, Eq)

data Term
        = Var Name
        | Let Prog Term
        | Type
        | Q PiSigma (Type, Bind Type)
        | Lam (Bind Term)
        | App Term Term
        | Pair Term Term
        | Split Term (Bind (Bind Term))
        | Enum [Name]
        | Label Label
        | Case Term [(Label, Term)]
        | Lift Term
        | Box Term
        | Force Term
        | Rec Term
        | Fold Term
        | Unfold Term (Bind Term)
        deriving (Show, Eq)

instance Pretty Term