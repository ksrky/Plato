{-# LANGUAGE LambdaCase #-}

module Plato.Core.Eval where

import Control.Exception.Safe
import Control.Monad.Reader
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Core.Data
import Plato.Core.Scope
import Plato.Syntax.Core

getIndex :: (MonadReader env m, MonadThrow m) => Ident -> Scope -> m Index
getIndex id sc = case lookupScope id sc of
        Just i -> return i
        Nothing -> throwError $ hsep ["Not in scope", pretty id]

lookupIndex :: (MonadReader env m, Env env, MonadIO m) => Index -> m EnvEntry
lookupIndex i = getE i =<< ask

evalIndex :: (MonadReader env m, Env env, MonadThrow m, MonadIO m) => Index -> m Val
evalIndex i =
        lookupIndex i >>= \case
                Index j -> return $ Ne (NVar j)
                Closure t -> eval t

decl ::
        (MonadReader e m, Env e, MonadIO m) =>
        Ident ->
        PrtInfo ->
        Scope ->
        Maybe (Clos Type) ->
        m (Index, Scope)
decl id fi sc a = do
        i <- extendE fi =<< ask
        return (i, extendScope id (i, a) sc)

decl' :: (MonadReader e m, Env e, MonadIO m) => Ident -> Scope -> m (Index, Scope)
decl' x s = decl x PrtInfo{name = x, expand = True} s Nothing

defn :: (MonadReader e m, Env e, MonadIO m) => Index -> EnvEntry -> m ()
defn i ei = setE i ei =<< ask

defn' :: (MonadReader e m, Env e, MonadIO m) => Index -> Clos Type -> m ()
defn' i = defn i . Closure

subst :: (MonadReader e m, Env e, MonadIO m) => Bind (Clos Term) -> Clos Term -> m (Clos Term)
subst (x, (t, sc)) u = do
        (i, s') <- decl' x sc
        defn' i u
        return (t, s')

unfold :: (MonadReader e m, Env e, MonadThrow m, MonadIO m) => Bind (Clos Term) -> Val -> m Val
unfold b (VFold c) = eval =<< subst b c
unfold b (Ne n) = return (Ne (NUnfold n b))
unfold _ _ = throwError "Fold expected"

eval :: forall e m. (MonadReader e m, Env e, MonadThrow m, MonadIO m) => Clos Term -> m Val
eval (Var id, sc) = evalIndex =<< getIndex id sc
eval (Let prog t, sc) = evalProg (prog, sc) >>= curry eval t
eval (Type, _) = return VType
eval (Q ps bind body, sc) = return (VQ ps ((bind, body), sc))
eval (Lam (x, _) t, sc) = return $ VLam (x, (t, sc)) (t, sc)
eval (App t u, sc) = eval (t, sc) >>= evalApp (u, sc)
    where
        evalApp :: Clos Term -> Val -> m Val
        evalApp u (VLam (x, _) t) = eval =<< subst (x, t) u
        evalApp u (Ne t) = return (Ne (t :.. u))
        evalApp _ _ = throwError "function expected"
eval (Pair t u, sc) = return $ VPair ((t, u), sc)
eval (Split t (x, (y, u)), sc) = eval (t, sc) >>= evalSplit (x, (y, (u, sc)))
    where
        evalSplit :: Bind (Bind (Clos Term)) -> Val -> m Val
        evalSplit (x, (y, (t, sc))) (VPair ((l, r), sc')) = do
                ts2 <- subst (x, (t, sc)) (l, sc')
                eval =<< subst (y, ts2) (r, sc')
        evalSplit binds (Ne ne) = return (Ne (NSplit ne binds))
        evalSplit _ _ = throwError "Pair expected"
eval (Enum ls, _) = return $ VEnum ls
eval (Label l, _) = return (VLabel l)
eval (Lift t, s) = return (VLift (t, s))
eval (Case t lts, sc) = eval (t, sc) >>= evalCase (lts, sc)
    where
        evalCase :: Clos [(Label, Term)] -> Val -> m Val
        evalCase (lts, s) (VLabel l) =
                case lookup l lts of
                        Nothing -> throwError "case not matched"
                        Just t -> eval (t, s)
        evalCase lts (Ne n) = return (Ne (NCase n lts))
        evalCase _ _ = throwError "Label expected"
eval (Box t, sc) = return (VBox (Boxed (t, sc)))
eval (Force t, s) = force =<< eval (t, s)
    where
        force :: Val -> m Val
        force (VBox (Boxed c)) = eval c
        force (Ne n) = return (Ne (NForce n))
        force _ = throwError "Box expected"
eval (Rec t, s) = return (VRec (t, s))
eval (Fold t, s) = return (VFold (t, s))
eval (Unfold (x, t) u, sc) = unfold (x, (u, sc)) =<< eval (t, sc)

evalProg :: (MonadReader e m, Env e, MonadIO m, MonadThrow m) => Clos Prog -> m Scope
evalProg ([], sc) = return sc
evalProg ((Decl id _) : tel, sc) = do
        (_, sc') <- decl id (PrtInfo id False) sc Nothing
        evalProg (tel, sc')
evalProg ((Defn id t) : tel, sc) = do
        i <- getIndex id sc
        defn' i (t, sc)
        evalProg (tel, sc)