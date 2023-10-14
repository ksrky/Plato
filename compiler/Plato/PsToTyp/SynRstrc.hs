module Plato.PsToTyp.SynRstrc (
        decNamesUnique,
        argNamesUnique,
        argPatsUnique,
        dataConUnique,
        dataConType,
        checkNumArgs,
) where

import Control.Exception.Safe
import Data.List qualified

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.PsToTyp.Utils
import Plato.Syntax.Parsing

-- | RULE 1: Declared name uniqueness
decNamesUnique :: (MonadThrow m) => [Ident] -> m ()
decNamesUnique = loop
    where
        loop :: (MonadThrow m) => [Ident] -> m ()
        loop [] = return ()
        loop (id1 : ids) = case Data.List.find (id1 ==) ids of
                Just id2 ->
                        throwLocErr (getLoc id2)
                                $ hsep ["Multiple declarations for", squotes $ pretty id2]
                Nothing -> loop ids

-- | RULE 2: Argument name uniqueness
argNamesUnique :: (MonadThrow m) => [Ident] -> m ()
argNamesUnique ids = do
        let dup = [(id1, id2) | id1 <- ids, id2 <- ids, stamp id1 /= stamp id2, nameIdent id1 == nameIdent id2]
        case dup of
                [] -> return ()
                (id1, id2) : _ ->
                        throwLocErr (getLoc id1 <> getLoc id2)
                                $ hsep ["Conflicting definitions for", squotes $ pretty id2]

argPatsUnique :: (MonadThrow m) => [LPat] -> m ()
argPatsUnique pats = do
        let ids = getDomain pats
        argNamesUnique ids

-- | RULE 3: Data constructor name uniqueness
dataConUnique :: (MonadThrow m) => [Ident] -> m ()
dataConUnique = decNamesUnique

-- | RULE 4: Restriction for datacon signature declarations
dataConType :: (MonadThrow m) => Ident -> (Ident, LType) -> m ()
dataConType id (con, ty) = loop1 ty
    where
        loop1 :: (MonadThrow m) => LType -> m ()
        loop1 (L sp AllT{}) = throwLocErr sp "A data constructor is not allowed to have polytype."
        loop1 (L _ (ArrT _ ty2)) = loop1 ty2
        loop1 (L _ (BinT _ op _))
                | nameIdent id == nameIdent op = return ()
        loop1 ty = loop2 ty
        loop2 :: (MonadThrow m) => LType -> m ()
        loop2 (L _ (AppT ty1 _)) = loop2 ty1
        loop2 (L _ (ConT id2)) | nameIdent id == nameIdent id2 = return ()
        loop2 (L sp ty) =
                throwLocErr sp
                        $ hsep ["Data constructor", squotes $ pretty con, "returns", pretty ty, "instead of", pretty id]

-- | RULE 5: Checking number of arguments
checkNumArgs :: (MonadThrow m) => LocDecl -> m ()
checkNumArgs (FunBindD id clauses@((pats1, _) : _))
        | all (\(pats, _) -> length pats == length pats1) clauses = return ()
        | otherwise =
                throwLocErr (getLoc clauses)
                        $ hsep
                                [ "Each clause of the definition of"
                                , pretty id
                                , "has different number of arguments."
                                ]
checkNumArgs _ = return ()