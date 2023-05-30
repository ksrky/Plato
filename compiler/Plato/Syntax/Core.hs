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
        = -- | Term-level variable @x@
          TmVar !Int NameInfo
        | -- | Application @t t@
          TmApp !Term !Term
        | -- | Abstraction @λx:T.t@
          TmAbs NameInfo !Type !Term
        | -- | Term-type application @t \@T@
          TmTApp !Term !Type
        | -- | Term-type abstraction @λX:K.t@
          TmTAbs NameInfo !Kind !Term
        | -- | Let binding
          TmLet NameInfo !Term !Term
        | -- | Fix combinator @fix t@
          TmFix !Term
        | -- | Record projection @t.i@
          TmProj !Term !Int
        | -- | Record @{l_1:t_1...l_n:t_n}@
          TmRecord ![(Label, Term)]
        | -- | Injection @inj_i[T](t_1...t_n)@
          TmInj Int Type ![Term]
        | -- | Case @case t {l_1->t_1...l_n->t_n}@
          TmCase !Term ![(Label, Term)]
        | TmFold Type
        | TmUnfold Type
        deriving (Eq, Show)

data Type
        = -- | Type-level variable @X@
          TyVar !Int NameInfo
        | -- | Type of functions  @T->T@
          TyFun !Type !Type
        | -- | Universal type @∀X:K.T@
          TyAll NameInfo !Kind !Type
        | -- | Type-type application @T T@
          TyApp !Type !Type
        | -- | Type-type abstraction @ΛX:K.T@
          TyAbs NameInfo !Kind !Type
        | -- | Recursive type @μX:K.T@
          TyRec NameInfo !Kind !Type
        | -- | Type of record @{l_1:T_1...l_n:T_n}@
          TyRecord ![(Label, Type)]
        | -- | Sum type @T_1+...+T_n@
          TySum ![Type]
        deriving (Eq, Show)

data Kind
        = -- | Kind of proper types @*@
          KnStar
        | -- | Kind of type operators @K->K@
          KnFun !Kind !Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | TmVarBind !Type
        | TyVarBind !Kind
        | TmAbbBind !Term !Type
        | TyAbbBind !Type !Kind
        deriving (Show)

data Command
        = Bind NameInfo !Binding
        | Eval !Term
        deriving (Show)

instance Pretty Term where
        pretty (TmVar _ fi) = pretty $ actualName fi
        pretty t@TmApp{} = prTerm2 t
        pretty (TmAbs fi tyT1 t2) = hcat ["λ", pretty (actualName fi), colon, pretty tyT1, dot, space, pretty t2]
        pretty (TmTApp t1 tyT2) = prTerm1 t1 <> pretty tyT2
        pretty (TmTAbs fi knK1 t2) = hcat ["Λ", pretty (actualName fi), colon, pretty knK1, dot, space, pretty t2]
        pretty TmLet{} = undefined
        pretty (TmFix t) = "fix" <+> prTerm1 t
        pretty (TmProj t i) = hcat [prTerm1 t, dot, pretty i]
        pretty (TmRecord fields) =
                braces $
                        concatWith
                                (surround comma)
                                (map (\(l, t) -> hcat [pretty l, equals, pretty t]) fields)
        pretty (TmInj i tyT2 elims) =
                hcat
                        [ "inj_"
                        , pretty i
                        , brackets (pretty tyT2)
                        , parens (concatWith (surround dot) (map pretty elims))
                        ]
        pretty (TmCase t alts) =
                hsep
                        [ "case"
                        , pretty t
                        , braces $
                                concatWith
                                        (surround (space <> pipe <> space))
                                        (map (\(l, t) -> hsep [pretty l, "->", pretty t]) alts)
                        ]
        pretty (TmFold tyT) = hsep ["fold", brackets $ pretty tyT]
        pretty (TmUnfold tyT) = hsep ["unfold", brackets $ pretty tyT]

prTerm1 :: Term -> Doc ann
prTerm1 t@TmVar{} = pretty t
prTerm1 t@(TmInj _ _ ts) | null ts = pretty t
prTerm1 t@TmRecord{} = pretty t
prTerm1 t = parens $ pretty t

prTerm2 :: Term -> Doc ann
prTerm2 t = walk t []
    where
        walk :: Term -> [Term] -> Doc ann
        walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
        walk t' ts = prTerm1 t' <+> sep (map prTerm1 ts)

instance Pretty Type where
        pretty (TyVar _ fi) = pretty (actualName fi)
        pretty (TyFun tyT1 tyT2) = hcat [pretty tyT1, "->", pretty tyT2]
        pretty (TyAll fi knK1 tyT2) = hcat ["∀", pretty (actualName fi), pretty knK1, dot, space, pretty tyT2]
        pretty (TyApp tyT1 tyT2) = pretty tyT1 <+> parens (pretty tyT2)
        pretty (TyAbs fi knK1 tyT2) = hcat ["λ", pretty (actualName fi), colon, pretty knK1, dot, space, pretty tyT2]
        pretty (TyRec fi knK1 tyT2) = hcat ["μ", pretty (actualName fi), colon, pretty knK1, dot, space, pretty tyT2]
        pretty (TyRecord fields) =
                braces $
                        concatWith
                                (surround $ comma <> space)
                                (map (\(l, ty) -> hcat [pretty l, colon, pretty ty]) fields)
        pretty (TySum fields) = parens $ concatWith (surround pipe) (map pretty fields)

instance Pretty Kind where
        pretty KnStar = "*"
        pretty (KnFun knK1 knK2) = hcat [pretty knK1, "->", pretty knK2]

instance Pretty Binding where
        pretty NameBind = emptyDoc
        pretty (TmVarBind tyT) = colon <+> pretty tyT
        pretty (TyVarBind knK) = colon <+> pretty knK
        pretty (TmAbbBind t _) = equals <+> pretty t
        pretty (TyAbbBind tyT _) = equals <+> pretty tyT

instance Pretty Command where
        pretty (Bind fi bind) = pretty (actualName fi) <+> pretty bind
        pretty (Eval t) = pretty t