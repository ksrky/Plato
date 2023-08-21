{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Plato.Core.Normalise where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Ident
import Plato.Common.Uniq
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Result
import Plato.Syntax.Core
import Plato.Syntax.Core.Helper

type Vars = [Ident]

class Nf a b | a -> b where
        nf :: (MonadReader e m, CoreEnv e, HasUniq e, MonadThrow m, MonadIO m) => Vars -> a -> m b
        nf = nf' True
        quote :: (MonadReader e m, CoreEnv e, HasUniq e, MonadThrow m, MonadIO m) => Vars -> a -> m b
        quote = nf' False
        nf' :: (MonadReader e m, CoreEnv e, HasUniq e, MonadThrow m, MonadIO m) => Bool -> Vars -> a -> m b

instance Nf (Clos Term) Term where
        nf' True xs t = nf' True xs =<< eval t
        nf' False xs t = qq xs t

instance Nf Index Term where
        nf' b xs i = do
                (PrtInfo x shouldExpand) <- prtE i =<< ask
                lookupIndex i >>= \case
                        (Index _) -> return (Var x)
                        (Closure t)
                                | shouldExpand -> do
                                        t' <- nf' b xs t
                                        return (Let [Defn x t'] (Var x))
                                | otherwise -> -- we should also declare x, but we don't know its type!
                                -- the let cannot be expanded if inside a box!
                                        return (Var x)

qq :: (MonadReader e m, CoreEnv e, HasUniq e, MonadThrow m, MonadIO m) => Vars -> Clos Term -> m Term
qq xs (Var x, s) = quote xs =<< getIndex x s
-- qq _  (Let _ _ _, _) = return (Label  "*quote-let-not-implemented*")
qq xs (Let g t, s) = do
        s' <- evalProg (g, s)
        qq (xs ++ decls g) (t, s')
qq xs (Q ps (x, a) b, s) = do
        a' <- qq xs (a, s)
        (x', b') <- quote xs (x, (b, s))
        return (Q ps (x', a') b')
qq xs (Lift t, s) = Lift <$> qq xs (t, s)
qq xs (Rec t, s) = Rec <$> qq xs (t, s)
qq xs (Fold t, s) = Fold <$> qq xs (t, s)
qq xs (Unfold (x, t) u, s) = do
        t' <- qq xs (t, s)
        (x', u') <- quote xs (x, (u, s))
        return (Unfold (x', t') u')
qq xs (Lam (x, ty) t, s) = do
        (x', t') <- quote xs (x, (t, s))
        return (Lam (x', ty) t')
qq xs (App t u, s) = do
        t' <- qq xs (t, s)
        u' <- qq xs (u, s)
        return (App t' u')
qq xs (Pair t u, s) = do
        t' <- qq xs (t, s)
        u' <- qq xs (u, s)
        return (Pair t' u')
qq xs (Split t (x, y) u, s) = do
        t' <- qq xs (t, s)
        (x', (y', u')) <- quote xs (x, (y, (u, s)))
        return (Split t' (x', y') u')
qq xs (Case t lts, s) = do
        t' <- qq xs (t, s)
        lts' <- forM lts $ \(l', t'') -> do
                t''' <- qq xs (t'', s)
                return (l', t''')
        return (Case t' lts')
qq xs (Box t, s) = Box <$> qq xs (t, s)
qq xs (Force t, s) = Force <$> qq xs (t, s)
qq _ (t, _) = return t -- Type, Enum, Label

instance (Closure a, Nf a b) => Nf (Ident, a) (Ident, b) where
        nf' b xs (x, t) = do
                x' <- reassignUniq x
                (_, s') <- decl x (PrtInfo x' True) (getScope t) Nothing
                t' <- nf' b (x' : xs) (putScope t s')
                return (x', t')

instance Nf Val Term where
        nf' b xs (Ne n) = nf' b xs n
        nf' _ _ VType = return Type
        nf' b xs (VQ ps (((x, a), c), s)) = do
                a' <- nf' b xs (a, s)
                (x', c') <- nf' b xs (x, (c, s))
                return (Q ps (x', a') c')
        nf' b xs (VLift c) = Lift <$> nf' b xs c
        nf' b xs (VRec c) = Rec <$> nf' b xs c
        nf' b xs (VFold c) = Fold <$> nf' b xs c
        nf' b xs (VLam (((x, ty), t), sc)) = do
                (x', t') <- nf' b xs (x, (t, sc))
                return (Lam (x', ty) t')
        nf' b xs (VPair ((t, u), s)) = do
                t' <- nf' b xs (t, s)
                u' <- nf' b xs (u, s)
                return (Pair t' u')
        nf' _ xs (VBox (Boxed c)) = Box <$> nf' False xs c
        nf' _ _ (VEnum ls) = return (Enum ls)
        nf' _ _ (VLabel l) = return (Label l)

instance Nf Ne Term where
        nf' b xs (NVar i) = nf' b xs i
        nf' b xs (t :.. u) = do
                t' <- nf' b xs t
                u' <- nf' b xs u
                return (App t' u')
        nf' b xs (NSplit t xyu) = do
                t' <- nf' b xs t
                (x', (y', u')) <- nf' b xs xyu
                return (Split t' (x', y') u')
        nf' b xs (NCase t (lus, s)) = do
                t' <- nf xs t
                lus' <- forM lus $ \(l, u) -> do
                        u' <- nf' b xs (u, s)
                        return (l, u')
                return (Case t' lus')
        nf' _ xs (NForce t) = Force <$> nf xs t
        nf' b xs (NUnfold t xu) = do
                t' <- nf' b xs t
                (x', u') <- nf' b xs xu
                return (Unfold (x', t') u')
