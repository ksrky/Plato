{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing where

import Plato.Common.GenName
import Plato.Common.Name
import Plato.Common.SrcLoc
import {-# SOURCE #-} Plato.Typing.KindInfer
import {-# SOURCE #-} Plato.Typing.TcTypes

import qualified Data.Map.Strict as M

data Expr
        = VarE GenName
        | AbsE GenName (Maybe Type) Expr
        | AppE Expr Expr
        | TAbsE [GenName] Expr
        | TAppE Expr [Type]
        | LetE [FuncD] Expr
        | ProjE Expr GenName
        | RecordE [(GenName, Expr)]
        | CaseE Expr (Maybe Type) [(Pat, Expr)]
        | TagE GenName [Expr] (Maybe Type)
        | FoldE Type
        | AnnE Expr Type {-Sigma-}
        deriving (Eq, Show)

data Pat
        = VarP GenName
        | ConP GenName [Pat]
        | WildP
        deriving (Eq, Show)

data Type
        = VarT TyVar
        | ConT GenName
        | ArrT Type Type
        | AllT [(TyVar, Maybe Kind)] Type {- Rho -}
        | AbsT GenName (Maybe Kind) Type
        | AppT Type Type
        | RecT GenName Type
        | RecordT [(GenName, Type)]
        | SumT [(GenName, [Type])]
        | MetaT MetaTv
        deriving (Eq, Show)

data Kind
        = MetaK MetaKv
        | StarK
        | ArrK Kind Kind
        deriving (Eq, Show)

data FuncD = FuncD GenName Expr Type deriving (Eq, Show)

data Decl
        = TypeD GenName Type
        | VarD GenName Type
        | ConD FuncD
        deriving (Eq, Show)

data Program = Program
        { mmodule :: Maybe ModuleName
        , decls :: [Located Decl]
        , binds :: [FuncD]
        , body :: [Located Expr]
        }
        deriving (Eq, Show)

type TypEnv = M.Map GenName Type {- Sigma -}