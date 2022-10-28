{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc
import {-# SOURCE #-} Plato.Typing.KindInfer
import {-# SOURCE #-} Plato.Typing.TcTypes

import qualified Data.Map.Strict as M

data Expr
        = VarE GlbName
        | AbsE GlbName (Maybe Type) Expr
        | AppE Expr Expr
        | TAbsE [GlbName] Expr
        | TAppE Expr [Type]
        | LetE [FuncD] Expr
        | ProjE Expr GlbName
        | RecordE [(GlbName, Expr)]
        | CaseE Expr (Maybe Type) [(Pat, Expr)]
        | TagE GlbName [Expr] (Maybe Type)
        | FoldE Type
        | AnnE Expr Type {-Sigma-}
        deriving (Eq, Show)

data Pat
        = VarP GlbName
        | ConP GlbName [Pat]
        | WildP
        deriving (Eq, Show)

data Type
        = VarT TyVar
        | ConT GlbName
        | ArrT Type Type
        | AllT [(TyVar, Maybe Kind)] Type {- Rho -}
        | AbsT GlbName (Maybe Kind) Type
        | AppT Type Type
        | RecT GlbName Type
        | RecordT [(GlbName, Type)]
        | SumT [(GlbName, [Type])]
        | MetaT MetaTv
        deriving (Eq, Show)

data Kind
        = MetaK MetaKv
        | StarK
        | ArrK Kind Kind
        deriving (Eq, Show)

data FuncD = FuncD GlbName Expr Type deriving (Eq, Show)

data Decl
        = TypeD GlbName Type
        | VarD GlbName Type
        | ConD FuncD
        deriving (Eq, Show)

data Program = Program
        { mmodule :: Maybe ModuleName
        , decls :: [Located Decl]
        , binds :: [FuncD]
        , body :: [Located Expr]
        }
        deriving (Eq, Show)

type TypEnv = M.Map GlbName Type {- Sigma -}