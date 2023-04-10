{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env where

import Control.Exception.Safe
import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Common.Error
import Plato.Syntax.Typing.Ident as Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Module
import Plato.Syntax.Typing.Path as Path
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
findBinding (Path.PIdent id) env = Ident.lookup id env
findBinding (Path.PDot root field) env = do
        find root env >>= \case
                Signature specs -> case findField field specs of
                        Just bndng -> return bndng
                        Nothing -> throwLocErr (Ident.span field) $ hsep [squotes $ pretty field, "is not in module", squotes $ pretty root]

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
                        _ -> throwLocErr (Path.getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

instance EnvManager Kind where
        extend id kn = M.insert id (TypeBinding kn)
        find p env =
                findBinding p env >>= \case
                        TypeBinding ty -> return ty
                        _ -> throwLocErr (Path.getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

instance EnvManager Signature where
        extend id sig = M.insert id (ModuleBinding sig)
        find p env =
                findBinding p env >>= \case
                        ModuleBinding ty -> return ty
                        _ -> throwLocErr (Path.getLoc p) $ hsep ["Not in scope ", squotes $ pretty p]

extendSpec :: Spec -> Env -> Env
extendSpec (ValueSpec id ty) = extend id ty
extendSpec (TypeSpec id kn) = extend id kn

extendSpecs :: [Spec] -> Env -> Env
extendSpecs = flip $ foldr extendSpec

{-}
outermost :: TypEnv
outermost = unreachable "Outermost of TypEnv"

emptyTypEnv :: TypEnv
emptyTypEnv = TypEnv M.empty M.empty M.empty

newTypEnv :: ModuleName -> TypEnv -> TypEnv
newTypEnv modn typenv =
        foldr
                ( \n env@(TypEnv _ _ modenv) -> case M.lookup n modenv of
                        Just _ -> env
                        Nothing -> extendEnv (noLoc n) env emptyTypEnv
                )
                typenv
                names
    where
        names = modn2names modn

class EnvManager a where
        extendEnv :: LName -> a -> TypEnv -> TypEnv
        extendEnvList :: [(LName, a)] -> TypEnv -> TypEnv
        lookupEnv :: MonadThrow m => LName -> TypEnv -> m a
        extendEnvList binds m = foldl (\m (x, v) -> extendEnv x v m) m binds

instance EnvManager Type where
        extendEnv x ty (TypEnv tyenv knenv modenv) = TypEnv (M.insert (unLoc x) ty tyenv) knenv modenv
        lookupEnv (L sp x) (TypEnv tyenv _ _) = case M.lookup x tyenv of
                Just ty -> return ty
                Nothing -> throwLocErr sp $ hsep ["Not in scope ", squotes $ pretty x]

instance EnvManager Kind where
        extendEnv x kn (TypEnv tyenv knenv modenv) = TypEnv tyenv (M.insert (unLoc x) kn knenv) modenv
        lookupEnv (L sp x) (TypEnv _ knenv _) = case M.lookup x knenv of
                Just kn -> return kn
                Nothing -> throwLocErr sp $ hsep ["Not in scope ", squotes $ pretty x]

{-
instance EnvManager Decl where
        extendEnv x (ValDecl ty) (TypEnv tyenv knenv modenv) = TypEnv (M.insert (unLoc x) ty tyenv) knenv modenv
        extendEnv x (TypDecl kn) (TypEnv tyenv knenv modenv) = TypEnv tyenv (M.insert (unLoc x) kn knenv) modenv
        lookupEnv (L sp x) (TypEnv tyenv knenv _) = case M.lookup x tyenv of
                Just ty -> return $ ValDecl ty
                Nothing -> case M.lookup x knenv of
                        Just kn -> return $ TypDecl kn
                        Nothing -> throwLocErr sp $ hsep ["Not in scope ", squotes $ pretty x]-}

instance EnvManager TypEnv where
        extendEnv x typenv (TypEnv tyenv knenv modenv) = TypEnv tyenv knenv (M.insert (unLoc x) typenv modenv)
        lookupEnv (L sp x) (TypEnv _ _ modenv) = case M.lookup x modenv of
                Just typenv -> return typenv
                Nothing -> throwLocErr sp $ hsep ["Not in scope ", squotes $ pretty x]
-}