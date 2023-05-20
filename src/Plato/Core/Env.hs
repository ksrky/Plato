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

addName :: Name -> CoreEnv -> CoreEnv
addName x = addBinding x NameBind

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        TmVarBind tyT -> TmVarBind (shift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (shift d t) (shift d tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (shift d tyT) opt

getBinding :: Int -> CoreEnv -> Binding
getBinding i env = bindingShift (i + 1) (snd $ env V.! i)