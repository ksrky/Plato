module Plato.Syntax.Core where

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Common.Pretty

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
        | Split Term (Ident, Ident) Term
        | Enum [Label]
        | Label Label
        | Case Term [(Label, Term)]
        | Lift Term
        | Box Term
        | Force Term
        | Rec Term
        | Fold Term
        | Unfold (Ident, Term) Term
        deriving (Show, Eq)

instance Pretty Entry where
        pretty (Decl x ty) = prettyId x <+> colon <+> hang 2 (pretty ty)
        pretty (Defn x t) = prettyId x <+> equals <+> hang 2 (pretty t)

prettyBind :: PrettyWithContext a => Int -> Bind a -> Doc ann
prettyBind p (id, ty)
        | nameIdent id == wcName = pretty' p ty
        | otherwise = parens $ hsep [prettyId id, colon, pretty' 0 ty]

instance Pretty Term where
        pretty = pretty' 0

instance PrettyWithContext Term where
        pretty' _ (Var id) = prettyId id
        pretty' p (Let prog t) =
                parenswPrec p 0 $ do
                        group $ align $ "let" <+> bindings prog <> line <> "in" <+> pretty' 0 t
            where
                bindings = align . encloseSep lbrace (space <> rbrace) semi . map (indent 1 . pretty)
        pretty' _ Type = "Type"
        pretty' p (Q Pi bind ty) =
                parenswPrec p 0 $ group $ hang 2 $ prettyBind 1 bind <> line <> arrow <+> pretty' 0 ty
        pretty' p (Q Sigma bind ty) =
                parenswPrec p 0 $ group $ hang 2 $ prettyBind 1 bind <> line <> asterisk <+> pretty' 0 ty
        pretty' p (Lam (x, ty) t) =
                parenswPrec p 0 $ group $ do
                        backslash <> prettyId x <> colon <+> pretty' 1 ty <> dot <> softline <> pretty' 0 t
        pretty' p (App t1 t2) =
                group $ hang 2 $ parenswPrec p 1 $ hsep [pretty' 1 t1, pretty' 2 t2]
        pretty' _ (Pair t1 t2) = tupled $ map (pretty' 0) [t1, t2]
        pretty' p (Split t (x, y) u) =
                parenswPrec p 0 $ hang 2 $ group $ do
                        "split" <+> pretty' 0 t <+> "with" <+> tuple <+> arrow <> softline <> pretty' 0 u
            where
                tuple = parens $ prettyId x <> comma <+> prettyId y
        pretty' _ (Enum labs) = braces $ map pretty labs `sepBy` comma
        pretty' _ (Label lab) = "`" <> pretty lab
        pretty' p (Case t lts) =
                parenswPrec p 0 $ group $ "case" <+> pretty' 0 t <+> "of" <> line <> branches lts
            where
                branch (l, t) = indent 1 $ hsep [pretty l, arrow, pretty' 0 t]
                branches = encloseSep lbrace (space <> rbrace) comma . map branch
        pretty' p (Lift t) = parenswPrec p 1 $ "^" <> pretty' 2 t
        pretty' _ (Box t) = brackets $ pretty' 0 t
        pretty' p (Force t) = parenswPrec p 1 $ "!" <> pretty' 2 t
        pretty' p (Rec t) = parenswPrec p 1 $ "Rec" <+> pretty' 2 t
        pretty' p (Fold t) = parenswPrec p 1 $ "fold" <+> pretty' 2 t
        pretty' p (Unfold (x, t) u) =
                parenswPrec p 0 $ hang 2 $ do
                        hsep ["unfold", pretty' 0 t, "as", prettyId x, "->", pretty' 0 u]
