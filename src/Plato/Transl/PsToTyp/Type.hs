module Plato.Transl.PsToTyp.Type where

import Control.Exception.Safe

import Plato.Common.Location
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

elabType :: MonadThrow m => P.LType -> m T.LType
elabType (L sp ty) = L sp <$> tratype ty
    where
        tratype :: MonadThrow m => P.Type -> m T.Type
        tratype (P.VarT id) = return $ T.VarT (T.BoundTv id)
        tratype (P.ConT path) = return $ T.ConT path
        tratype (P.AppT ty1 ty2) = do
                ty1' <- elabType ty1
                ty2' <- elabType ty2
                return $ T.AppT ty1' ty2'
        tratype (P.ArrT ty1 ty2) = do
                ty1' <- elabType ty1
                ty2' <- elabType ty2
                return $ T.ArrT ty1' ty2'
        tratype (P.AllT xs ty1) = do
                ty1' <- elabType ty1
                return $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) xs) ty1'