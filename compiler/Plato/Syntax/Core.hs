module Plato.Syntax.Core (
        module Plato.Syntax.Core.Info,
        Term (..),
        Type (..),
        Kind (..),
        Binding (..),
        Command (..),
) where

import Prettyprinter

import Plato.Common.Name
import Plato.Syntax.Core.Info

type Label = Name

data Term
        = -- | Term-level variable
          TmVar !Int NameInfo
        | -- | Application
          TmApp Term Term
        | -- | Abstraction
          TmAbs NameInfo Type Term
        | -- | Type application
          TmTApp Term Type
        | -- | Type abstraction
          TmTAbs NameInfo Kind Term
        | -- | Let binding
          TmLet NameInfo Term Term
        | -- | Fix combinator
          TmFix Term
        | -- | Record projection
          TmProj Term Int
        | -- | Record
          TmRecord [(Label, Term)]
        | -- | Injection to variants
          TmInj Int Term Type
        | -- | Case tree
          TmCase Term [(Label, Term)]
        deriving (Eq, Show)

data Type
        = TyVar !Int NameInfo
        | TyFun Type Type
        | TyAll NameInfo Kind Type
        | TyApp Type Type
        | TyAbs NameInfo Kind Type
        | TyRec NameInfo Kind Type
        | TyRecord [(Label, Type)]
        | TySum [Type]
        deriving (Eq, Show)

data Kind = KnStar | KnFun Kind Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | TmVarBind Type
        | TyVarBind Kind
        | TmAbbBind Term Type
        | TyAbbBind Type Kind

data Command
        = Bind NameInfo Binding
        | Eval Term

instance Pretty Term where
        pretty (TmVar i _) = pretty i
        pretty (TmApp t1 t2) = parens $ pretty t1 <> parens (pretty t2)
        pretty (TmAbs _ tyT1 t2) = parens $ hcat [pipe, pretty tyT1, dot, pretty t2]
        pretty (TmTApp t1 tyT2) = parens $ pretty t1 <> parens (pretty tyT2)
        pretty (TmTAbs _ knK1 t2) = parens $ hcat [pipe, pretty knK1, pretty t2]
        pretty TmLet{} = undefined
        pretty (TmFix t) = parens $ "fix" <> parens (pretty t)
        pretty (TmProj t i) = hcat [pretty t, dot, pretty i]
        pretty (TmRecord fields) =
                hcat [lbrace, concatWith (surround comma) (map (pretty . snd) fields), rbrace]
        pretty (TmInj i t1 tyT2) = hcat ["inj_", pretty i, "^", pretty tyT2, parens (pretty t1)]
        pretty (TmCase t alts) =
                hcat ["case_", pretty t, hcat (map (\(_, t) -> pipe <> pretty t) alts)]

instance Pretty Type
instance Pretty Kind