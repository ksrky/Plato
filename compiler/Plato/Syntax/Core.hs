module Plato.Syntax.Core where

import Prettyprinter

import Plato.Common.Name

-- *  Abstract syntax

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
        | Q PiSigma (Bind Type) Type
        | Lam (Bind Type) Term
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
        | Unfold (Bind Term) Term
        deriving (Show, Eq)

instance Pretty Entry where
        pretty (Decl x ty) = hsep [pretty x, colon, pretty ty]
        pretty (Defn x t) = hsep [pretty x, equals, pretty t]

instance Pretty Term where
        pretty (Var x) = pretty x
        pretty (Let prog t) = hsep ["let", braces $ hsep (map pretty prog), pretty t]
        pretty Type = "Type"
        pretty (Q Pi (x, ty) t) = hsep [parens (hsep [pretty x, colon, pretty ty]), "->", pretty t]
        pretty (Q Sigma (x, ty) t) = hsep [parens (hsep [pretty x, colon, pretty ty]), "*", pretty t]
        pretty (Lam (x, ty) t) = hsep ["\\", pretty x, colon, pretty ty, dot, pretty t]
        pretty (App t u) = ppr TopPrec t <+> ppr AppPrec u
        pretty (Pair t u) = parens (pretty t <> comma <+> pretty u)
        pretty (Split t (x, (y, u))) =
                hsep
                        [ "split"
                        , ppr TopPrec t
                        , "with"
                        , parens (pretty x <> comma <+> pretty y)
                        , "->"
                        , pretty u
                        ]
        pretty (Enum labs) = braces $ concatWith (surround (comma <> space)) (map pretty labs)
        pretty (Label lab) = "`" <> pretty lab
        pretty (Case t alts) =
                hsep
                        [ "case"
                        , ppr TopPrec t
                        , braces $
                                concatWith
                                        (surround (semi <> space))
                                        (map (\(l, t) -> hsep [pretty l, "->", pretty t]) alts)
                        ]
        pretty (Lift t) = "^" <> pretty t
        pretty (Box t) = brackets $ pretty t
        pretty (Force t) = "!" <> pretty t
        pretty (Rec t) = "Rec" <+> pretty t
        pretty (Fold t) = "fold" <+> pretty t
        pretty (Unfold (x, t) u) = hsep ["unfold", ppr TopPrec t, "as", pretty x, "->", pretty u]

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