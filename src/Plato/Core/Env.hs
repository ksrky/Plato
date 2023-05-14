module Plato.Core.Context where

import Control.Monad.Reader
import Data.Vector qualified as V

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Uniq
import Plato.Core.Calc
import Plato.Syntax.Core

type CoreEnv = V.Vector (Uniq, Binding)

class HasCoreEnv a where
        getEnv :: a -> CoreEnv
        modifyEnv :: (CoreEnv -> CoreEnv) -> a -> a

instance HasCoreEnv CoreEnv where
        getEnv = id
        modifyEnv = id

emptyEnv :: CoreEnv
emptyEnv = V.empty

lookupEnv :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> m Binding
lookupEnv id = asks (loop . getEnv)
    where
        loop :: CoreEnv -> Binding
        loop env = case V.uncons env of
                Just ((k, b), rest)
                        | stamp id == k -> b
                        | otherwise -> loop rest
                Nothing -> unreachable "Core.Env.lookupEnv"

addBinding :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> Binding -> m a -> m a
addBinding id bind = local (modifyEnv $ V.cons (stamp id, bind))

addName :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> m a -> m a
addName id = addBinding id NameBind

addNameList :: (MonadReader ctx m, HasCoreEnv ctx) => [Ident] -> m a -> m a
addNameList = flip $ foldr addName

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (shift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (shift d t) (shift d tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (shift d tyT) opt

getBinding :: (MonadReader ctx m, HasCoreEnv ctx) => Int -> m Binding
getBinding i = do
        env <- asks getEnv
        return $ bindingShift (i + 1) (snd $ env V.! i)

getType :: (MonadReader ctx m, HasCoreEnv ctx) => Int -> m Type
getType i = do
        bind <- getBinding i
        case bind of
                VarBind tyT -> return tyT
                TmAbbBind _ tyT -> return tyT
                _ -> unreachable "Plato.Core.Env.getType"

getKind :: (MonadReader ctx m, HasCoreEnv ctx) => Int -> m Kind
getKind i = do
        bind <- getBinding i
        case bind of
                TyVarBind knK -> return knK
                TyAbbBind _ knK -> return knK
                _ -> unreachable "Plato.Core.Env.getKind"

getVarIndex :: (MonadReader ctx m, HasCoreEnv ctx) => Ident -> m Int
getVarIndex id = do
        env <- asks getEnv
        case V.elemIndex (stamp id) (V.map fst env) of
                Just i -> return i
                Nothing -> unreachable "Plato.Core.Env.getVarIndex"