{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        Bind (..),
        TypEnv,
        initTypEnv,
        HasTypEnv (..),
        EnvManager (..),
        Constrs,
        ConEnv,
        initConEnv,
        HasConEnv (..),
        extendConEnv,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Typing.Utils

data Bind
        = ValBind Type
        | TypBind Kind
        deriving (Eq, Show)

type TypEnv = IdentMap Bind

initTypEnv :: TypEnv
initTypEnv = M.empty

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
        extendList :: [(Ident, a)] -> TypEnv -> TypEnv
        find :: MonadThrow m => Ident -> TypEnv -> m a
        extendList l env = foldr (uncurry extend) env l

instance EnvManager a => EnvManager (Located a) where
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

-----------------------------------------------------------
-- Constructor Env
-----------------------------------------------------------
type Constrs = [(Ident, [Type])]

-- | Mapping a type constructor to data constructors
type ConEnv = IdentMap Constrs

initConEnv :: ConEnv
initConEnv = M.empty

class HasConEnv a where
        getConEnv :: a -> ConEnv
        modifyConEnv :: (ConEnv -> ConEnv) -> a -> a

instance HasConEnv ConEnv where
        getConEnv = id
        modifyConEnv = id

extendConEnv :: HasConEnv env => Ident -> [(Ident, LType)] -> env -> env
extendConEnv id constrs =
        modifyConEnv $ M.insert id (map (\(con, ty) -> (con, fst $ splitConstrTy (unLoc ty))) constrs)