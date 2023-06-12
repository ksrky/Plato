{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        Constrs,
        Binding (..),
        TypEnv,
        initTypEnv,
        HasTypEnv (..),
        EnvManager (..),
        extendConEnv,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing

type Constrs = [(Ident, [Type])]

data Binding
        = ValBinding Type
        | TypBinding Kind
        | ConBinding Constrs
        deriving (Eq, Show)

type TypEnv = IdentMap Binding

initTypEnv :: TypEnv
initTypEnv = M.empty

class HasTypEnv a where
        getEnv :: Monad m => a -> m TypEnv
        modifyEnv :: (TypEnv -> TypEnv) -> a -> a

instance HasTypEnv TypEnv where
        getEnv = return
        modifyEnv = id

class EnvManager a where
        extend :: Ident -> a -> TypEnv -> TypEnv
        extendList :: [(Ident, a)] -> TypEnv -> TypEnv
        find :: MonadThrow m => Ident -> TypEnv -> m a
        extendList l env = foldr (uncurry extend) env l

instance EnvManager a => EnvManager (Located a) where
        extend id x = extend id (unLoc x)
        find = ((noLoc <$>) .) . find

instance EnvManager Type where
        extend id ty = M.insert id (ValBinding ty)
        find id env =
                lookupIdent id env >>= \case
                        ValBinding ty -> return ty
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a term-level identifier"]

instance EnvManager Kind where
        extend id kn = M.insert id (TypBinding kn)
        find id env =
                lookupIdent id env >>= \case
                        TypBinding kn -> return kn
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a type-level identifier"]

instance EnvManager Constrs where
        extend id constrs = modifyEnv $ M.insert id (ConBinding constrs)
        find id env =
                lookupIdent id env >>= \case
                        ConBinding constrs -> return constrs
                        _ -> throwLocErr (getLoc id) $ hsep [squotes $ pretty id, "is not a constructor"]

extendConEnv :: HasTypEnv env => Ident -> [(Ident, LType)] -> env -> env
extendConEnv id constrs =
        modifyEnv $ extend id (map (\(con, ty) -> (con, split [] (unLoc ty))) constrs)
    where
        split :: [Sigma] -> Rho -> [Sigma]
        split acc (ArrT sigma rho) = split (unLoc sigma : acc) (unLoc rho)
        split acc _ = acc