module Plato.Core.Result where

import Plato.Common.Ident
import Plato.Common.Pretty
import Plato.Core.Closure
import Plato.Syntax.Core

newtype Boxed = Boxed (Clos Term) deriving (Eq, Show)

instance Closure Boxed where
        getScope (Boxed c) = getScope c
        putScope (Boxed c) = Boxed . putScope c

data Val
        = Ne Ne
        | VType
        | VQ PiSigma (Clos (Bind Type, Type))
        | VLift (Clos Type)
        | VLam (Clos (Bind Type, Term))
        | VPair (Clos (Term, Term))
        | VEnum [Label]
        | VLabel Label
        | VBox Boxed
        | VRec (Clos Type)
        | VFold (Clos Term)
        deriving (Eq, Show)

-- | Neutral terms.
data Ne
        = NVar Index
        | Ne :.. (Clos Term)
        | NSplit Ne (Bind (Bind (Clos Term)))
        | NCase Ne (Clos [(Label, Term)])
        | NForce Ne
        | NUnfold Ne (Bind (Clos Term))
        deriving (Eq, Show)

infix 5 :..

instance Pretty Boxed where
        pretty (Boxed (t, _)) = brackets $ pretty t

instance Pretty Val where
        pretty = pretty' 0

instance PrettyWithContext Val where
        pretty' c (Ne ne) = pretty' c ne
        pretty' _ VType = "Type"
        pretty' c (VQ Pi ((bind, ty), _)) =
                contextParens c 0 $ hsep [prettyBind 1 bind, "->", pretty' 0 ty]
        pretty' c (VQ Sigma ((bind, ty), _)) =
                contextParens c 0 $ hsep [prettyBind 1 bind, "*", pretty' 0 ty]
        pretty' c (VLam (((x, ty), t), _)) =
                contextParens c 0 $ hsep ["\\", prettyId x, colon, pretty' 1 ty, dot, pretty' 0 t]
        pretty' _ (VPair ((t, u), _)) = parens $ map (pretty' 0) [t, u] `sepBy` comma
        pretty' _ (VEnum labs) = braces $ map pretty labs `sepBy` comma
        pretty' _ (VLabel lab) = "`" <> pretty lab
        pretty' c (VLift t) = contextParens c 1 $ "^" <> pretty' 2 t
        pretty' _ (VBox t) = brackets $ pretty t
        pretty' c (VRec (ty, _)) = contextParens c 1 $ "Rec" <+> pretty' 2 ty
        pretty' c (VFold (t, _)) = contextParens c 1 $ "fold" <+> pretty' 2 t

instance Pretty Ne where
        pretty = pretty' 0

instance PrettyWithContext Ne where
        pretty' _ (NVar i) = pretty i
        pretty' _ (ne :.. (t, _)) = hsep [pretty ne, ":..", pretty t]
        pretty' c (NSplit ne (x, (y, (t, _)))) =
                contextParens c 0 $
                        hang 2 $
                                hsep
                                        [ "split"
                                        , pretty' 0 ne
                                        , "with"
                                        , parens $ [prettyId x, prettyId y] `sepBy` comma
                                        , "->"
                                        , pretty' 0 t
                                        ]
        pretty' _ (NCase ne (lts, _)) =
                hang 1 $
                        hsep
                                [ "case"
                                , pretty' 0 ne
                                , "of"
                                , braces $ map (\(l, t) -> hsep [pretty l, "->", pretty' 0 t]) lts `sepBy` semi
                                ]
        pretty' c (NForce ne) = contextParens c 1 $ "!" <> pretty' 2 ne
        pretty' c (NUnfold ne (x, (t, _))) =
                contextParens c 0 $
                        hang 2 $
                                hsep ["unfold", pretty' 0 t, "as", prettyId x, "->", pretty' 0 ne]
