module Plato.Core.Equal where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Error
import Plato.Common.Pretty
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Result
import Plato.Syntax.Core

class Equal a where
        (~) :: (MonadReader e m, CoreEnv e, MonadThrow m, MonadIO m) => a -> a -> m ()

instance Equal (Clos Term) where
        (~) t u = do
                t' <- eval t
                u' <- eval u
                t' ~ u'

infix 1 ~

-- unused?
-- eq' :: Env e => Clos (Term,Term) -> Eval e ()
-- eq' ((t,u),s) = eq (t,s) (u,s)

eqBind ::
        (MonadReader e m, CoreEnv e, MonadIO m, Closure a) =>
        (a -> a -> m ()) ->
        Bind a ->
        Bind a ->
        m ()
eqBind eqArg (x0, c0) (x1, c1) = do
        let s0 = getScope c0
            s1 = getScope c1
        (i, s0') <- decl' x0 s0
        let s1' = extendScope x1 (i, Nothing) s1
        let c0' = putScope c0 s0'
            c1' = putScope c1 s1'
        eqArg c0' c1'

instance (Equal a, Closure a) => Equal (Bind a) where
        (~) = eqBind (~)

instance Equal Val where
        Ne t0 ~ Ne t1 = t0 ~ t1
        VQ ps0 (((x0, a0), b0), s0) ~ VQ ps1 (((x1, a1), b1), s1) | ps0 == ps1 = do
                (a0, s0) ~ (a1, s1)
                (x0, (b0, s0)) ~ (x1, (b1, s1))
        VLam (((x0, _), b0), s0) ~ VLam (((x1, _), b1), s1) = do
                (x0, (b0, s0)) ~ (x1, (b1, s1))
        VPair ((t0, u0), s0) ~ VPair ((t1, u1), s1) = do
                (t0, s0) ~ (t1, s1)
                (u0, s0) ~ (u1, s1)
        VBox b ~ VBox b' = b ~ b'
        VLift a ~ VLift a' = a ~ a'
        VRec a ~ VRec a' = a ~ a'
        VFold a ~ VFold a' = a ~ a'
        v0 ~ v1
                | v0 == v1 = return () -- Type, Label, Enum
                | otherwise = throwError "Different values"

{- eqBox implements alpha equality -}
eqBox :: (MonadReader e m, CoreEnv e, MonadThrow m, MonadIO m) => Clos Term -> Clos Term -> m ()
-- eqBox c c' | c == c' = return ()
eqBox (Var x, s) (Var y, s') = do
        x' <- getIndex x s
        y' <- getIndex y s'
        x' ~ y'
eqBox (Let p t, s) c = do
        s' <- evalProg (p, s)
        eqBox (t, s') c
eqBox c c'@(Let _ _, _) = eqBox c' c
eqBox (Q ps x a b, s) (Q ps' x' a' b', s') | ps == ps' = do
        eqBox (a, s) (a', s')
        (x, Boxed (b, s)) ~ (x', Boxed (b', s'))
eqBox (Lam (x, _) t, s) (Lam (x', _) t', s') = (x, Boxed (t, s)) ~ (x', Boxed (t', s'))
eqBox (App t u, s) (App t' u', s') = do
        eqBox (t, s) (t', s')
        eqBox (u, s) (u', s')
eqBox (Pair t u, s) (Pair t' u', s') = do
        eqBox (t, s) (t', s')
        eqBox (u, s) (u', s')
eqBox (Split t (x, y) u, s) (Split t' (x', y') u', s') = do
        eqBox (t, s) (t', s')
        (x, (y, Boxed (u, s))) ~ (x', (y', Boxed (u', s')))
eqBox (Case t bs, s) (Case t' bs', s') = do
        eqBox (t, s) (t', s')
        zipWithM_
                ( \(l, t'') (l', t''') ->
                        if l == l'
                                then eqBox (t'', s) (t''', s')
                                else throwError "eqBox case"
                )
                bs
                bs'
eqBox (Lift t, s) (Lift t', s') = eqBox (t, s) (t', s')
eqBox (Box t, s) (Box t', s') = eqBox (t, s) (t', s')
eqBox (Force t, s) (Force t', s') = eqBox (t, s) (t', s')
eqBox (Rec t, s) (Rec t', s') = eqBox (t, s) (t', s')
eqBox (Fold t, s) (Fold t', s') = eqBox (t, s) (t', s')
eqBox (Unfold t, s) (Unfold t', s') = eqBox (t, s) (t', s')
eqBox (t, _) (t', _)
        | t == t' = return () -- Type, Label, Enum
        | otherwise = throwError "Different terms"

instance Equal Boxed where
        Boxed c ~ Boxed c' = eqBox c c'

instance Equal Ix where
        i0 ~ i1
                | i0 == i1 = return ()
                | otherwise = do
                        ei0 <- lookupIndex i0
                        ei1 <- lookupIndex i1
                        case (ei0, ei1) of
                                (Index j0, Index j1) ->
                                        unless
                                                (j0 == j1)
                                                (throwError "Different variables")
                                (Closure t0, Closure t1) ->
                                        letn
                                                i0
                                                (Index i0)
                                                ( letn
                                                        i1
                                                        (Index i0)
                                                        (t0 ~ t1)
                                                )
                                _ -> throwError "Variable vs neutral"

instance Equal Ne where
        NVar i0 ~ NVar i1 = i0 ~ i1
        t0 :.. u0 ~ t1 :.. u1 = do
                t0 ~ t1
                u0 ~ u1
        NSplit t0 xyu0 ~ NSplit t1 xyu1 = do
                t0 ~ t1
                xyu0 ~ xyu1
        NCase t0 (lus0, s0) ~ NCase t1 (lus1, s1) = do
                t0 ~ t1
                eqBranches lus0 lus1
            where
                eqBranches [] [] = return ()
                eqBranches ((l0, u0) : lus0') ((l1, u1) : lus1')
                        | l0 == l1 = do
                                (u0, s0) ~ (u1, s1)
                                eqBranches lus0' lus1'
                eqBranches _ _ = throwError "Case: branches differ"
        NForce t ~ NForce t' = t ~ t'
        NUnfold t ~ NUnfold t' = t ~ t'
        t ~ u = throwError $ hsep ["Different neutrals:", pretty t, "/=", pretty u]