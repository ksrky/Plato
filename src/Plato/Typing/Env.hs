module Plato.Typing.Env where

import Control.Exception.Safe
import Control.Monad
import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Typing

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
