{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env where

import Control.Exception.Safe
import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Module
import Plato.Syntax.Typing.Type

data Binding
        = ValueBinding Type
        | TypeBinding Kind
        | ModuleBinding Signature
        deriving (Eq, Show)

type Env = IdentMap Binding

class EnvManager a where
        extend :: Ident -> a -> Env -> Env
        extendList :: [(Ident, a)] -> Env -> Env
        find :: MonadThrow m => Path -> Env -> m a
        extendList l env = foldl (\env (x, v) -> extend x v env) env l

findBinding :: MonadThrow m => Path -> Env -> m Binding
findBinding (PIdent id) env = lookupIdent id env
findBinding (PDot root field) env = do
        find root env >>= \case
                Signature specs -> case findField field specs of
                        Just bndng -> return bndng
                        Nothing -> throwLocErr (spanIdent field) $ hsep [squotes $ pretty field, "is not in module", squotes $ pretty root]

findField :: Ident -> [Spec] -> Maybe Binding
findField _ [] = Nothing
findField id1 (ValueSpec id2 ty : _) | id1 == id2 = Just $ ValueBinding ty
findField id1 (TypeSpec id2 kn : _) | id1 == id2 = Just $ TypeBinding kn
findField id (_ : rest) = findField id rest

instance EnvManager Type where
        extend id ty = M.insert id (ValueBinding ty)
        find p env =
                findBinding p env >>= \case
                        ValueBinding ty -> return ty
                        _ -> throwLocErr (getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

instance EnvManager Kind where
        extend id kn = M.insert id (TypeBinding kn)
        find p env =
                findBinding p env >>= \case
                        TypeBinding kn -> return kn
                        _ -> throwLocErr (getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

instance EnvManager Signature where
        extend id sig = M.insert id (ModuleBinding sig)
        find p env =
                findBinding p env >>= \case
                        ModuleBinding sig -> return sig
                        _ -> throwLocErr (getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

extendSpec :: Spec -> Env -> Env
extendSpec (ValueSpec id ty) = extend id ty
extendSpec (TypeSpec id kn) = extend id kn

extendSpecs :: [Spec] -> Env -> Env
extendSpecs = flip $ foldr extendSpec