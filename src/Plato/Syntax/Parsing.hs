module Plato.Syntax.Parsing where

import Plato.Common.Name
import Plato.Common.SrcLoc

type PsLName = Located Name
type PsLExpr = Located Expr
type PsLPat = Located Pat
type PsLType = Located Type
type PsLDecl = Located Decl

data Expr
        = VarExpr PsLName
        | AppExpr PsLExpr PsLExpr
        | OpExpr PsLExpr PsLName PsLExpr
        | LamExpr [PsLName] PsLExpr
        | LetExpr [PsLDecl] PsLExpr
        | CaseExpr PsLExpr [(PsLPat, PsLExpr)]
        | Factor PsLExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConPat PsLName [PsLPat]
        | VarPat PsLName
        | WildPat
        deriving (Eq, Show)

data Type
        = VarType PsLName
        | AppType PsLType PsLType
        | ArrType PsLType PsLType
        | AllType [PsLName] PsLType
        deriving (Eq, Show)

data Decl
        = FuncDecl PsLName [PsLName] PsLExpr
        | FuncTyDecl PsLName PsLType
        deriving (Eq, Show)

data TopDecl
        = DataDecl PsLName [PsLName] [(PsLName, [PsLType])]
        | TypeDecl PsLName [PsLName] PsLType
        | Decl PsLDecl
        | FixDecl
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl (Located ModuleName) deriving (Eq, Show)

data Program = Program
        { moduleDecl :: Maybe (Located ModuleName)
        , importDecls :: [Located ImpDecl]
        , topDecls :: [Located TopDecl]
        }
        deriving (Show)
