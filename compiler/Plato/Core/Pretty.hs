module Plato.Core.Pretty where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Uniq
import Plato.Core.Data
import Plato.Core.Normalise
import Plato.Syntax.Core

class Print a where
        evalPrint :: (MonadReader e m, Env e, HasUniq e, MonadThrow m, MonadIO m) => a -> m (Doc ann)

instance Print Ident where
        evalPrint = return . prettyId

instance Print Val where
        evalPrint a = evalPrint =<< quote [] a

instance Print (Clos Term) where
        evalPrint a = evalPrint =<< quote [] a

instance Print Ne where
        evalPrint a = evalPrint =<< quote [] a

instance Print Term where
        evalPrint = return . prettyTerm 0

prettyTerm :: Int -> Term -> Doc ann
prettyTerm _ (Var id) = prettyId id
prettyTerm c (Let prog t) =
        contextParens c 0 $
                hsep
                        [ "let"
                        , braces $ map pretty prog `sepBy` semi
                        , "in"
                        , prettyTerm 0 t
                        ]
prettyTerm _ Type = "Type"
prettyTerm c (Q Pi bind ty) =
        contextParens c 0 $
                hsep [parens (binding 1 bind), "->", prettyTerm 0 ty]
prettyTerm c (Q Sigma bind ty) =
        contextParens c 0 $
                hsep [parens (binding 1 bind), "*", prettyTerm 0 ty]
prettyTerm c (Lam (x, ty) t) =
        contextParens c 0 $
                hsep ["\\", prettyId x, colon, prettyTerm 1 ty, "->", prettyTerm 0 t]
prettyTerm c (App t1 t2) =
        group $
                hang 2 $
                        contextParens c 1 $
                                hsep [prettyTerm 1 t1, prettyTerm 2 t2]
prettyTerm _ (Pair t1 t2) = parens $ map (prettyTerm 0) [t1, t2] `sepBy` comma
prettyTerm c (Split t (x, (y, u))) =
        contextParens c 0 $
                hang 2 $
                        hsep
                                [ "split"
                                , prettyTerm 0 t
                                , "with"
                                , parens $ [prettyId x, prettyId y] `sepBy` comma
                                , "->"
                                , prettyTerm 0 u
                                ]
prettyTerm _ (Enum labs) = braces $ map pretty labs `sepBy` comma
prettyTerm _ (Label lab) = "`" <> pretty lab
prettyTerm _ (Case t lts) =
        hang 1 $
                hsep
                        [ "case"
                        , prettyTerm 0 t
                        , "of"
                        , map (\(l, t) -> hsep [pretty l, "->", pretty t]) lts `sepBy` semi
                        ]
prettyTerm c (Lift t) = contextParens c 1 $ "^" <> pretty t
prettyTerm _ (Box t) = brackets $ prettyTerm 0 t
prettyTerm c (Force t) = contextParens c 1 $ "!" <> prettyTerm 2 t
prettyTerm c (Rec t) = contextParens c 1 $ "Rec" <+> prettyTerm 2 t
prettyTerm c (Fold t) = contextParens c 1 $ "fold" <+> prettyTerm 2 t
prettyTerm c (Unfold (x, t) u) =
        contextParens c 0 $
                hang 2 $
                        hsep ["unfold", prettyTerm 0 t, "as", prettyId x, "->", prettyTerm 0 u]

binding :: Int -> Bind Type -> Doc ann
binding c (id, ty)
        | nameIdent id == wcName = prettyTerm c ty
        | otherwise = parens $ hsep [prettyId id, colon, prettyTerm 0 ty]

prettyEntry :: Entry -> Doc ann
prettyEntry (Decl x ty) = hang 2 $ hsep [prettyId x, colon, prettyTerm 0 ty]
prettyEntry (Defn x t) = hang 2 $ hsep [prettyId x, equals, pretty t]