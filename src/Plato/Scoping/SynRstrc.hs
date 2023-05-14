module Plato.Scoping.SynRstrc (
        defNamesUnique,
        paramNamesUnique,
        paramPatsUnique,
        dataConUnique,
        dataConType,
) where

import Control.Exception.Safe
import Data.List qualified
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Scoping.Utils
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
        let dup = [(id1, id2) | id1 <- ids, id2 <- ids, nameIdent id1 == nameIdent id2]
        case dup of
                [] -> return ()
                (id1, id2) : _ ->
                        throwLocErr (combineSpans (getLoc id1) (getLoc id2)) $
                                hsep ["Conflicting definitions for", squotes $ pretty id2]

paramPatsUnique :: MonadThrow m => [LPat] -> m ()
paramPatsUnique pats = do
        let ids = allIdentsFromPats pats
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
        loop2 (L _ (ConT id2)) | nameIdent id == nameIdent id2 = return ()
        loop2 (L sp ty) =
                throwLocErr sp $
                        hsep ["Data constructor", squotes $ pretty con, "returns", pretty ty]