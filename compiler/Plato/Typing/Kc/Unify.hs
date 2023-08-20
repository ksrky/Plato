module Plato.Typing.Kc.Unify (
        unify,
        unifyFun,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Kc.Utils

unify :: (MonadThrow m, MonadIO m) => Span -> Kind -> Kind -> m ()
unify sp = unify'
    where
        unify' :: (MonadThrow m, MonadIO m) => Kind -> Kind -> m ()
        unify' StarK StarK = return ()
        unify' (ArrK l r) (ArrK l' r') = do
                unify' l l'
                unify' r r'
        unify' (MetaK kv1) (MetaK kv2) | kv1 == kv2 = return ()
        unify' (MetaK kv) kn = unifyVar kv kn
        unify' kn (MetaK kv) = unifyVar kv kn
        unify' kn1 kn2 = do
                throwLocErr sp $ vsep ["Couldn't match kind.", "Expected kind:" <+> pretty kn2, indent 2 ("Actual kind:" <+> pretty kn1)]

        unifyVar :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> m ()
        unifyVar kv1 kn2@(MetaK kv2) = do
                mb_kn1 <- readMetaKv kv1
                mb_kn2 <- readMetaKv kv2
                case (mb_kn1, mb_kn2) of
                        (Just kn1, _) -> unify' kn1 kn2
                        (Nothing, Just kn2) -> unify' (MetaK kv1) kn2
                        (Nothing, Nothing) -> writeMetaKv kv1 kn2
        unifyVar kv1 kn2 = do
                occursCheck kv1 kn2
                writeMetaKv kv1 kn2
        occursCheck :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> m ()
        occursCheck kv1 kn2 = do
                kvs2 <- getMetaKvs kn2
                when (kv1 `S.member` kvs2) $ throwLocErr sp $ hsep ["Infinite kind:", squotes $ pretty kn2]

unifyFun :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Span -> Kind -> m (Kind, Kind)
unifyFun _ (ArrK kn1 kn2) = return (kn1, kn2)
unifyFun sp kn = do
        arg_kn <- newKnVar
        res_kn <- newKnVar
        unify sp kn (ArrK arg_kn res_kn)
        return (arg_kn, res_kn)