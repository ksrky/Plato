{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        Constrs,
        TypEnv,
        HasTypEnv (..),
        EnvManager (..),
        extendQuants,
        extendBinds,
        extendDataBinds,
        envTypes,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data Signat
        = ValBind Type
        | TypBind Kind
        deriving (Eq, Show)

type Signats = IdentMap Signat

type Constrs = [(Ident, [Type])]

type DataBind = ([TyVar], Constrs)

type DataBinds = IdentMap DataBind

data TypEnv = TypEnv {typ_signats :: Signats, typ_databinds :: DataBinds}
        deriving (Eq, Show)

instance Semigroup TypEnv where
        TypEnv s1 d1 <> TypEnv s2 d2 = TypEnv (s1 <> s2) (d1 <> d2)

instance Monoid TypEnv where
        mempty = TypEnv mempty mempty

class HasTypEnv a where
        getTypEnv :: a -> TypEnv
        modifyTypEnv :: (TypEnv -> TypEnv) -> a -> a
        setTypEnv :: TypEnv -> a -> a
        setTypEnv = modifyTypEnv . const
        modifySignats :: (Signats -> Signats) -> a -> a
        modifySignats f = modifyTypEnv $ \env -> env{typ_signats = f $ typ_signats env}
        modifyDataBinds :: (DataBinds -> DataBinds) -> a -> a
        modifyDataBinds f = modifyTypEnv $ \env -> env{typ_databinds = f $ typ_databinds env}

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
        extend id ty = modifySignats $ M.insert id (ValBind ty)
        find id env =
                lookupIdent id (typ_signats env) >>= \case
                        ValBind ty -> return ty
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a term-level identifier"]

instance EnvManager Kind where
        extend id kn = modifySignats $ M.insert id (TypBind kn)
        find id env =
                lookupIdent id (typ_signats env) >>= \case
                        TypBind kn -> return kn
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a type-level identifier"]

instance EnvManager DataBind where
        extend id bind = modifyDataBinds $ M.insert id bind
        find id env = lookupIdent id (typ_databinds env)

extendQuants :: Quants -> TypEnv -> TypEnv
extendQuants qns = extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qns)

extendBinds :: XBinds 'Typed -> TypEnv -> TypEnv
extendBinds binds = extendList $ fmap (\(Bind (id, ty) _) -> (id, ty)) binds

extendDataBinds :: TypDefn -> TypEnv -> TypEnv
extendDataBinds (DatDefn id qns ctors) =
        extend id (map fst qns, map (\(con, ty) -> (con, fst $ splitConstrTy (unLoc ty))) ctors)

envTypes :: TypEnv -> [Type]
envTypes = M.elems . M.mapMaybe (\case ValBind ty -> Just ty; _ -> Nothing) . typ_signats