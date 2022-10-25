module Plato.Syntax.Parsing where

import Plato.Common.Name
import Plato.Common.SrcLoc

type PsLName = Located Name
type PsLExpr = Located Expr
type PsLPat = Located Pat
type PsLType = Located Type
type PsLDecl = Located Decl

data Expr
        = VarE PsLName
        | AppE PsLExpr PsLExpr
        | OpE PsLExpr PsLName PsLExpr
        | LamE [PsLName] PsLExpr
        | LetE [PsLDecl] PsLExpr
        | CaseE PsLExpr [(PsLPat, PsLExpr)]
        | FactorE PsLExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConP PsLName [PsLPat]
        | VarP PsLName
        | WildP
        deriving (Eq, Show)

data Type
        = VarT PsLName
        | ConT PsLName
        | AppT PsLType PsLType
        | ArrT PsLType PsLType
        | AllT [PsLName] PsLType
        deriving (Eq, Show)

data Decl
        = FuncD PsLName [PsLName] PsLExpr
        | FuncTyD PsLName PsLType
        deriving (Eq, Show)

data TopDecl
        = DataD PsLName [PsLName] [(PsLName, [PsLType])]
        | TypeD PsLName [PsLName] PsLType
        | FixD
        | Decl PsLDecl
        | Eval PsLExpr
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl (Located ModuleName) deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe (Located ModuleName)
        , importDecls :: [Located ImpDecl]
        , topDecls :: [Located TopDecl]
        }
        deriving (Show)
