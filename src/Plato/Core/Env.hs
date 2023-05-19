{-# LANGUAGE FlexibleInstances #-}

module Plato.Core.Env where

import Control.Monad.Reader
import Data.Vector qualified as V
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Name
import Plato.Core.Calc
import Plato.Syntax.Core

type CoreEnv = V.Vector (Name, Binding)

class HasCoreEnv a where
        getEnv :: a -> CoreEnv
        modifyEnv :: (CoreEnv -> CoreEnv) -> a -> a

instance HasCoreEnv CoreEnv where
        getEnv = id
        modifyEnv = id

initCoreEnv :: CoreEnv
initCoreEnv = V.empty

lookupEnv :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => Ident -> m Binding
lookupEnv id = asks (loop . getEnv)
    where
        loop :: CoreEnv -> Binding
        loop env = case V.uncons env of
                Just ((x, bind), rest)
                        | nameIdent id == x -> bind
                        | otherwise -> loop rest
                Nothing -> unreachable "Core.Env.lookupEnv"

addBinding :: Name -> Binding -> CoreEnv -> CoreEnv
addBinding x bind = V.cons (x, bind)

extendWith :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> Binding -> m a -> m a
extendWith id bind = local (modifyEnv $ addBinding (nameIdent id) bind)

addNameWith :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> m a -> m a
addNameWith id = extendWith id NameBind

addNameListWith :: (MonadReader ctx m, HasCoreEnv ctx) => [Ident] -> m a -> m a
addNameListWith = flip $ foldr addNameWith

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        TmVarBind tyT -> TmVarBind (shift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (shift d t) (shift d tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (shift d tyT) opt

getBinding :: Int -> CoreEnv -> Binding
getBinding i env = bindingShift (i + 1) (snd $ env V.! i)

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

getVarIndex :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => Ident -> m Int
getVarIndex id = do
        env <- asks getEnv
        case V.elemIndex (nameIdent id) (V.map fst env) of
                Just idx -> return idx
                Nothing -> unreachable "Plato.Core.Env.getVarIndex"