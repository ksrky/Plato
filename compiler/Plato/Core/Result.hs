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
        = NVar Ix
        | Ne :.. (Clos Term)
        | NSplit Ne (Bind (Bind (Clos Term)))
        | NCase Ne (Clos [(Label, Term)])
        | NForce Ne
        | NUnfold Ne
        deriving (Eq, Show)

infix 5 :..

instance Pretty Ne where
        pretty = pretty' 0

instance PrettyWithContext Ne where
        pretty' _ (NVar i) = pretty i
        pretty' _ (ne :.. (t, _)) = hsep [pretty ne, ":..", pretty t]
        pretty' p (NSplit ne (x, (y, (t, _)))) =
                parenswPrec p 0 $ hang 2 $ do
                        hsep ["split", pretty' 0 ne, "with", tupled [prettyId x, prettyId y], "->", pretty' 0 t]
        pretty' _ (NCase ne (lts, _)) =
                hang 1 $
                        hsep
                                [ "case"
                                , pretty' 0 ne
                                , "of"
                                , encloseSep lbrace rbrace (semi <> space) $ map (\(l, t) -> hsep [pretty l, "->", pretty' 0 t]) lts
                                ]
        pretty' p (NForce ne) = parenswPrec p 1 $ "!" <> pretty' 2 ne
        pretty' p (NUnfold ne) = parenswPrec p 0 $ hang 2 $ hsep ["unfold", pretty' 2 ne]
