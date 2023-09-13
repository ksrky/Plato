{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Kc (checkKindStar, inferKind, checkKind, kcTypDefns) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable qualified as Foldable
import Data.Graph
import GHC.Stack

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Error
import Plato.Typing.Kc.Unify

checkKindStar :: (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) => LType -> m ()
checkKindStar ty = checkKind ty StarK

inferKind ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        LType ->
        m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        checkKind ty exp_kn
        return (ty, exp_kn)

checkKind ::
        forall e m.
        (HasCallStack, MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        LType ->
        Kind ->
        m ()
checkKind (L sp ty) exp_kn = case ty of
        VarT (FreeTv _) -> do
                unify StarK exp_kn --tmp
        VarT tv -> do
                kn <- find (unTyVar tv) =<< asks getTypEnv
                unify_ kn exp_kn
        ConT tc -> do
                kn <- find tc =<< asks getTypEnv
                unify_ kn exp_kn
        ArrT arg res -> do
                checkKindStar arg
                checkKindStar res
                unify_ exp_kn StarK
        AllT qns body -> do
                unify_ exp_kn StarK
                local (modifyTypEnv $ extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qns)) $
                        checkKindStar body
        AppT fun arg -> do
                arg_kn <- newKnVar
                checkKind fun (ArrK arg_kn exp_kn)
                checkKind arg arg_kn
        MetaT{} -> return ()
    where
        unify_ :: Kind -> Kind -> m ()
        unify_ kn1 kn2 = catches (unify kn1 kn2) (kcErrorHandler sp kn1 kn2)

kcTypDefn ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        TypDefn 'Untyped ->
        m (TypDefn 'Typed)
kcTypDefn (DatDefn id params constrs) = do
        let extenv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        local (modifyTypEnv extenv) $ mapM_ (checkKindStar . snd) constrs
        let kn = foldr (\(_, kn1) kn2 -> ArrK kn1 kn2) StarK params
        kn' <- find id =<< asks getTypEnv
        unify kn kn'
        return $ DatDefn' (id, kn) params constrs

kcTypDefns ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        SCC (TypDefn 'Untyped) ->
        m (SCC (TypDefn 'Typed))
kcTypDefns tdefs = do
        extenv <- extendList <$> mapM (\(DatDefn id _ _) -> (id,) <$> newKnVar) (Foldable.toList tdefs)
        local (modifyTypEnv extenv) $ mapM kcTypDefn tdefs