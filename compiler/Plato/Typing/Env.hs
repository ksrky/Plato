{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        TypEnv,
        HasTypEnv (..),
        EnvManager (..),
        extendQuants,
        extendBinds,
        extendPath,
        extendDataBinds,
        envTypes,
        Constrs,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Common.Pretty
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data Signat
        = ValBind Type
        | TypBind Kind
        | ModBind TypEnv
        deriving (Eq, Show)

type Signats = IdentMap Signat

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
        find :: (MonadThrow m) => Path -> TypEnv -> m a
        extendList l env = foldr (uncurry extend) env l
        findId :: (MonadThrow m) => Ident -> TypEnv -> m a
        findId id = find (PIdent id)

instance (EnvManager a) => EnvManager (Located a) where
        extend id x = extend id (unLoc x)
        find = ((noLoc <$>) .) . find

lookupPath :: forall m. (MonadThrow m) => Path -> TypEnv -> m Signat
lookupPath (PIdent id) env = lookupIdent id (typ_signats env)
lookupPath (path :.: id) env =
        lookupPath path env >>= \case
                ModBind env -> lookupIdent id (typ_signats env)
                _ -> unreachable "ModBind required"

instance EnvManager Type where
        extend id ty = modifySignats $ M.insert id (ValBind ty)
        find path env =
                lookupPath path env >>= \case
                        ValBind ty -> return ty
                        _ -> throwLocErr (getLoc path) $ hsep [squotes $ pretty path, "is not a term-level identifier"]

instance EnvManager Kind where
        extend id kn = modifySignats $ M.insert id (TypBind kn)
        find path env =
                lookupPath path env >>= \case
                        TypBind kn -> return kn
                        _ -> throwLocErr (getLoc path) $ hsep [squotes $ pretty path, "is not a type-level identifier"]

instance EnvManager TypEnv where
        extend id env = modifySignats $ M.insert id (ModBind env)
        find path env =
                lookupPath path env >>= \case
                        ModBind env' -> return env'
                        _ -> throwLocErr (getLoc path) $ hsep [squotes $ pretty path, "is not a module"]

extendQuants :: Quants -> TypEnv -> TypEnv
extendQuants qns = extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qns)

extendBinds :: XBinds 'Typed -> TypEnv -> TypEnv
extendBinds binds = extendList $ fmap (\(Bind (id, ty) _) -> (id, ty)) binds

extendPath :: (MonadThrow m) => Ident -> Path -> TypEnv -> m TypEnv
extendPath id path env = do
        env' <- find @TypEnv path env
        return $ extend @TypEnv id env' env

extendDataBinds :: TypDefn -> TypEnv -> TypEnv
extendDataBinds (DatDefn id qns ctors) =
        extend id (map fst qns, map (\(con, ty) -> (con, fst $ splitConstrTy (unLoc ty))) ctors)

envTypes :: TypEnv -> [Type]
envTypes = M.elems . M.mapMaybe (\case ValBind ty -> Just ty; _ -> Nothing) . typ_signats

-----------------------------------------------------------
-- Constructor Env
-----------------------------------------------------------
type Constrs = [(Ident, [Type])]

type DataBind = ([TyVar], Constrs)

type DataBinds = IdentMap DataBind

instance EnvManager DataBind where
        extend id bind = modifyDataBinds $ M.insert id bind
        find (PIdent id) env = lookupIdent id (typ_databinds env)
        find (root :.: field) env =
                lookupPath root env >>= \case
                        ModBind env' -> lookupIdent field (typ_databinds env')
                        _ -> throwLocErr (getLoc root) $ hsep [squotes $ pretty root, "is not a module"]

{-}
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
        constrs' = map (\(con, ty) -> (con, fst $ splitConstrTy (unLoc ty))) constrs-}