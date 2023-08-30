module Plato.Typing.Kc (checkKindStar, inferKind, checkKind) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import GHC.Stack

import Plato.Common.Error
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
        VarT (BoundTv id) -> do
                kn <- find id =<< asks getTypEnv
                unify_ kn exp_kn
        VarT FreeTv{} -> unreachable "Plato.KindCheck.Kc.checkKind passed FreeTv"
        ConT tc -> do
                kn <- find tc =<< asks getTypEnv
                unify_ kn exp_kn
        ArrT arg res -> do
                checkKindStar arg
                checkKindStar res
                unify_ exp_kn StarK
        AllT qnts body ->
                local (modifyTypEnv $ extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qnts)) $
                        checkKind body exp_kn
        AppT fun arg -> do
                arg_kn <- newKnVar
                checkKind fun (ArrK arg_kn exp_kn)
                checkKind arg arg_kn
        MetaT{} -> unreachable "Plato.KindInfer.Typ.checkKind received MetaT"
    where
        unify_ :: Kind -> Kind -> m ()
        unify_ kn1 kn2 = catches (unify kn1 kn2) (kcErrorHandler sp kn1 kn2)