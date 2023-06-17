{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTyp.SynRstrc (
        defNamesUnique,
        paramNamesUnique,
        paramPatsUnique,
        dataConUnique,
        dataConType,
        bundleClauses,
) where

import Control.Exception.Safe
import Control.Monad
import Data.List qualified
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.PsToTyp.Utils
import Plato.Syntax.Parsing

-- | RULE 1: Declared name uniqueness
defNamesUnique :: MonadThrow m => [Ident] -> m ()
defNamesUnique = loop
    where
        loop :: MonadThrow m => [Ident] -> m ()
        loop [] = return ()
        loop (id1 : ids) = case Data.List.find (id1 ==) ids of
                Just id2 ->
                        throwLocErr (getLoc id2) $
                                hsep ["Multiple declarations for", squotes $ pretty id2]
                Nothing -> loop ids

-- | RULE 2: Paramter name uniqueness
paramNamesUnique :: MonadThrow m => [Ident] -> m ()
paramNamesUnique ids = do
        let dup = [(id1, id2) | id1 <- ids, id2 <- ids, stamp id1 /= stamp id2, nameIdent id1 == nameIdent id2]
        case dup of
                [] -> return ()
                (id1, id2) : _ ->
                        throwLocErr (combineSpans (getLoc id1) (getLoc id2)) $
                                hsep ["Conflicting definitions for", squotes $ pretty id2]

paramPatsUnique :: MonadThrow m => [LPat] -> m ()
paramPatsUnique pats = do
        let ids = getDomain pats
        paramNamesUnique ids

-- | RULE 3: Data constructor name uniqueness
dataConUnique :: MonadThrow m => [Ident] -> m ()
dataConUnique = defNamesUnique

-- | RULE 4: Constructor signature rule
dataConType :: MonadThrow m => Ident -> (Ident, LType) -> m ()
-- before scoping
dataConType id (con, ty) = loop1 ty
    where
        loop1 :: MonadThrow m => LType -> m ()
        loop1 (L _ (ArrT _ ty2)) = loop1 ty2
        loop1 ty = loop2 ty
        loop2 :: MonadThrow m => LType -> m ()
        loop2 (L _ (AppT ty1 _)) = loop2 ty1
        loop2 (L _ (InfixT _ op _))
                | nameIdent id == nameIdent op = return ()
                | otherwise = error $ show op ++ show id
        loop2 (L _ (ConT id2)) | nameIdent id == nameIdent id2 = return ()
        loop2 (L sp ty) = do
                throwLocErr sp $
                        hsep ["Data constructor", squotes $ pretty con, "returns", pretty ty]

{- | RULE 5: Bundling function clauses \\
Checking number of arguments
-}
bundleClauses :: MonadThrow m => [LDecl] -> m [LDecl]
bundleClauses = classify . partition

classify :: MonadThrow m => [[LDecl]] -> m [LDecl]
classify [] = return []
classify ([] : rest) = classify rest
classify (fspcs@(L _ FunSpecD{} : _) : rest) = (fspcs ++) <$> classify rest
classify (fbnds@(L sp (FunBindD id [(pats, _)]) : _) : rest) = do
        clses <-
                sequence
                        [ do
                                when (length psi /= length pats) $ throwLocErr sp "Different number of arguments"
                                return (psi, ei)
                        | L sp (FunBindD _ [(psi, ei)]) <- fbnds
                        ]
        let spn = concatSpans $ sp : [spi | L spi FunBindD{} <- fbnds]
        (L spn (FunBindD id clses) :) <$> classify rest
classify ((L _ FunBindD{} : _) : _) = unreachable "malformed clauses"
classify ((L _ FixityD{} : _) : _) = unreachable "deleted by Nicifier"
classify ((L _ DataD{} : _) : _) = unreachable "Data declaration allowed only top-level"

partition :: [LDecl] -> [[LDecl]]
partition =
        Data.List.groupBy
                ( curry $ \case
                        -- before scoping
                        (L _ (FunBindD id1 _), L _ (FunBindD id2 _)) -> nameIdent id1 == nameIdent id2
                        _ -> False
                )