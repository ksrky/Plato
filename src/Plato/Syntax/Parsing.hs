module Plato.Syntax.Parsing where

import Plato.Common.Name
import Plato.Common.SrcLoc

type LName = Located Name
type LExpr = Located Expr
type LPat = Located Pat
type LType = Located Type
type LDecl = Located Decl

data Expr
        = VarE LName
        | AppE LExpr LExpr
        | OpE LExpr LName LExpr
        | LamE [LName] LExpr
        | LetE [LDecl] LExpr
        | CaseE LExpr [(LPat, LExpr)]
        | FactorE LExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConP LName [LPat]
        | VarP LName
        | WildP
        deriving (Eq, Show)

data Type
        = VarT LName
        | ConT LName
        | AppT LType LType
        | ArrT LType LType
        | AllT [LName] LType
        deriving (Eq, Show)

data Decl
        = FuncD LName [LName] LExpr
        | FuncTyD LName LType
        deriving (Eq, Show)

data TopDecl
        = DataD LName [LName] [(LName, [LType])]
        | TypeD LName [LName] LType
        | FixD
        | Decl LDecl
        | Eval LExpr
        deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe (Located ModuleName)
        , importDecls :: [Located ModuleName]
        , topDecls :: [Located TopDecl]
        }
        deriving (Show)
