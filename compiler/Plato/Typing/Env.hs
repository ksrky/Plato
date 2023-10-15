{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        TypEnv,
        HasTypEnv (..),
        EnvManager (..),
        extendQuants,
        extendBinds,
        envTypes,
        Constrs,
        ConEnv,
        HasConEnv (..),
        extendConstrs,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data EnvBind
        = ValBind Type
        | TypBind Kind
        deriving (Eq, Show)

type TypEnv = IdentMap EnvBind

class HasTypEnv a where
        getTypEnv :: a -> TypEnv
        modifyTypEnv :: (TypEnv -> TypEnv) -> a -> a
        setTypEnv :: TypEnv -> a -> a
        setTypEnv = modifyTypEnv . const

instance HasTypEnv TypEnv where
        getTypEnv = id
        modifyTypEnv = id

class EnvManager a where
        extend :: Ident -> a -> TypEnv -> TypEnv
        extendList :: (Foldable t) => t (Ident, a) -> TypEnv -> TypEnv
        find :: (MonadThrow m) => Ident -> TypEnv -> m a
        extendList l env = foldr (uncurry extend) env l

instance (EnvManager a) => EnvManager (Located a) where
        extend id x = extend id (unLoc x)
        find = ((noLoc <$>) .) . find

instance EnvManager Type where
        extend id ty = M.insert id (ValBind ty)
        find id env =
                lookupIdent id env >>= \case
                        ValBind ty -> return ty
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a term-level identifier"]

instance EnvManager Kind where
        extend id kn = M.insert id (TypBind kn)
        find id env =
                lookupIdent id env >>= \case
                        TypBind kn -> return kn
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a type-level identifier"]

extendQuants :: Quants -> TypEnv -> TypEnv
extendQuants qns = extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qns)

extendBinds :: XBinds 'Typed -> TypEnv -> TypEnv
extendBinds binds = extendList $ fmap (\(Bind (id, ty) _) -> (id, ty)) binds

envTypes :: TypEnv -> [Type]
envTypes = M.elems . M.mapMaybe (\case ValBind ty -> Just ty; _ -> Nothing)

-----------------------------------------------------------
-- Constructor Env
-----------------------------------------------------------
type Constrs = [(Ident, [Type])]

-- | Mapping a type constructor to data constructors
type ConEnv = IdentMap ([TyVar], Constrs)

class HasConEnv a where
        getConEnv :: a -> ConEnv
        modifyConEnv :: (ConEnv -> ConEnv) -> a -> a
        setConEnv :: ConEnv -> a -> a
        setConEnv = modifyConEnv . const

instance HasConEnv ConEnv where
        getConEnv = id
        modifyConEnv = id

extendConstrs :: (HasConEnv e) => Ident -> [TyVar] -> [(Ident, LType)] -> e -> e
extendConstrs id params constrs = modifyConEnv $ M.insert id (params, constrs')
    where
        constrs' = map (\(con, ty) -> (con, fst $ splitConstrTy (unLoc ty))) constrs