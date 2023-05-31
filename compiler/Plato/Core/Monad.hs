module Plato.Core.Monad where

import Control.Monad.Reader
import Data.Vector qualified as V
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Name
import Plato.Core.Env
import Plato.Syntax.Core

class (MonadFail m, MonadReader env m, HasCoreEnv env) => CoreMonad env m

type Core = ReaderT CoreEnv IO
instance CoreMonad CoreEnv Core

runCore :: Core a -> CoreEnv -> IO a
runCore = runReaderT

extendWith :: (MonadReader r m, HasCoreEnv r) => Name -> Binding -> m a -> m a
extendWith x bind = local (modifyEnv $ addBinding x bind)

extendNameWith :: (MonadReader r m, HasCoreEnv r) => Name -> m a -> m a
extendNameWith x = extendWith x NameBind

extendNameListWith :: (MonadReader ctx m, HasCoreEnv ctx) => [Name] -> m a -> m a
extendNameListWith = flip $ foldr extendNameWith

getType :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => Int -> m Type
getType i = do
        bind <- asks (getBinding i . getEnv)
        case bind of
                TmVarBind tyT -> return tyT
                TmAbbBind _ tyT -> return tyT
                _ -> unreachable "Plato.Core.Env.getType"

getKind :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => Int -> m Kind
getKind i = do
        bind <- asks (getBinding i . getEnv)
        case bind of
                TyVarBind knK -> return knK
                TyAbbBind _ knK -> return knK
                _ -> unreachable "Plato.Core.Env.getKind"

getVarIndex :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => Name -> m Int
getVarIndex x = do
        env <- asks getEnv
        case V.elemIndex x (V.map fst env) of
                Just idx -> return idx
                Nothing -> unreachable $ "Unbound name, " ++ show x