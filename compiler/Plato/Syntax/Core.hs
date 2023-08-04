module Plato.Syntax.Core where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Name

-- *  Abstract syntax

data Phrase
        = Prog Prog
        | Term Term
        deriving (Show, Eq)

type Label = Name

data Entry
        = Decl Ident Type
        | Defn Ident Term
        deriving (Show, Eq)

type Prog = [Entry]

type Type = Term

type Bind a = (Ident, a)

data PiSigma
        = Pi
        | Sigma
        deriving (Show, Eq)

data Term
        = Var Ident
        | Let Prog Term
        | Type
        | Q PiSigma (Bind Type) Type
        | Lam (Bind Type) Term
        | App Term Term
        | Pair Term Term
        | Split Term (Ident, (Ident, Term))
        | Enum [Label]
        | Label Label
        | Case Term [(Label, Term)]
        | Lift Term
        | Box Term
        | Force Term
        | Rec Term
        | Fold Term
        | Unfold (Bind Term) Term
        deriving (Show, Eq)

instance Pretty Entry where
        pretty (Decl x ty) = hsep [prettyId x, colon, pretty ty]
        pretty (Defn x t) = hsep [prettyId x, equals, pretty t]

prettyBind :: Pretty a => Bind a -> Doc ann
prettyBind (id, ty) = hsep [prettyId id, colon, pretty ty]

instance Pretty Term where
        pretty (Var x) = prettyId x
        pretty (Let prog t) =
                hsep
                        [ "let"
                        , braces $ concatWith (surround (semi <> space)) (map pretty prog)
                        , "in"
                        , pretty t
                        ]
        pretty Type = "Type"
        pretty (Q Pi bind ty) = hsep [parens (prettyBind bind), "->", pretty ty]
        pretty (Q Sigma bind ty) = hsep [parens (prettyBind bind), "*", pretty ty]
        pretty (Lam bind t) = hsep ["\\", prettyBind bind, dot, pretty t]
        pretty (App t u) = ppr TopPrec t <+> ppr AppPrec u
        pretty (Pair t u) = parens (pretty t <> comma <+> pretty u)
        pretty (Split t (x, (y, u))) =
                hsep
                        [ "split"
                        , ppr TopPrec t
                        , "with"
                        , parens (prettyId x <> comma <+> prettyId y)
                        , "->"
                        , pretty u
                        ]
        pretty (Enum labs) = braces $ concatWith (surround (comma <> space)) (map pretty labs)
        pretty (Label lab) = "`" <> pretty lab
        pretty (Case t lts) =
                hsep
                        [ "case"
                        , ppr TopPrec t
                        , braces $
                                concatWith
                                        (surround (semi <> space))
                                        (map (\(l, t) -> hsep [pretty l, "->", pretty t]) lts)
                        ]
        pretty (Lift t) = "^" <> pretty t
        pretty (Box t) = brackets $ pretty t
        pretty (Force t) = "!" <> pretty t
        pretty (Rec t) = "Rec" <+> pretty t
        pretty (Fold t) = "fold" <+> pretty t
        pretty (Unfold (x, t) u) = hsep ["unfold", ppr TopPrec t, "as", prettyId x, "->", pretty u]

data Prec = TopPrec | AppPrec | AtomPrec deriving (Enum)

precOf :: Type -> Prec
precOf Var{} = AtomPrec
precOf Type{} = AtomPrec
precOf Enum{} = AtomPrec
precOf Pair{} = AtomPrec
precOf Label{} = AtomPrec
precOf App{} = AppPrec
precOf Lift{} = AppPrec
precOf Box{} = AppPrec
precOf Force{} = AppPrec
precOf Rec{} = AppPrec
precOf Fold{} = AppPrec
precOf _ = TopPrec

ppr :: Prec -> Term -> Doc ann
ppr p t
        | fromEnum p >= fromEnum (precOf t) = parens (pretty t)
        | otherwise = pretty t