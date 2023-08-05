module Plato.Syntax.Core where

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Common.Pretty

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

prettyBind :: PrettyWithContext a => Int -> Bind a -> Doc ann
prettyBind c (id, ty)
        | nameIdent id == wcName = pretty' c ty
        | otherwise = parens $ hsep [prettyId id, colon, pretty' 0 ty]

instance Pretty Term where
        pretty = pretty' 0

instance PrettyWithContext Term where
        pretty' _ (Var id) = prettyId id
        pretty' c (Let prog t) =
                contextParens c 0 $
                        hsep
                                [ "let"
                                , braces $ map pretty prog `sepBy` semi
                                , "in"
                                , pretty' 0 t
                                ]
        pretty' _ Type = "Type"
        pretty' c (Q Pi bind ty) =
                contextParens c 0 $ hsep [prettyBind 1 bind, "->", pretty' 0 ty]
        pretty' c (Q Sigma bind ty) =
                contextParens c 0 $ hsep [prettyBind 1 bind, "*", pretty' 0 ty]
        pretty' c (Lam (x, ty) t) =
                contextParens c 0 $ hsep ["\\", prettyId x, colon, pretty' 1 ty, dot, pretty' 0 t]
        pretty' c (App t1 t2) =
                group $ hang 2 $ contextParens c 1 $ hsep [pretty' 1 t1, pretty' 2 t2]
        pretty' _ (Pair t1 t2) = parens $ map (pretty' 0) [t1, t2] `sepBy` comma
        pretty' c (Split t (x, (y, u))) =
                contextParens c 0 $
                        hang 2 $
                                hsep
                                        [ "split"
                                        , pretty' 0 t
                                        , "with"
                                        , parens $ [prettyId x, prettyId y] `sepBy` comma
                                        , "->"
                                        , pretty' 0 u
                                        ]
        pretty' _ (Enum labs) = braces $ map pretty labs `sepBy` comma
        pretty' _ (Label lab) = "`" <> pretty lab
        pretty' _ (Case t lts) =
                hang 1 $
                        hsep
                                [ "case"
                                , pretty' 0 t
                                , "of"
                                , braces $ map (\(l, t) -> hsep [pretty l, "->", pretty' 0 t]) lts `sepBy` semi
                                ]
        pretty' c (Lift t) = contextParens c 1 $ "^" <> pretty' 2 t
        pretty' _ (Box t) = brackets $ pretty' 0 t
        pretty' c (Force t) = contextParens c 1 $ "!" <> pretty' 2 t
        pretty' c (Rec t) = contextParens c 1 $ "Rec" <+> pretty' 2 t
        pretty' c (Fold t) = contextParens c 1 $ "fold" <+> pretty' 2 t
        pretty' c (Unfold (x, t) u) =
                contextParens c 0 $
                        hang 2 $
                                hsep ["unfold", pretty' 0 t, "as", prettyId x, "->", pretty' 0 u]
