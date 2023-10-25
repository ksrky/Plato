{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Kc.Unify (unify) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Set               qualified as S

import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Error
import Plato.Typing.Misc

unify :: (MonadThrow m, MonadIO m) => Kind -> Kind -> m ()
unify StarK StarK = return ()
unify (ArrK l r) (ArrK l' r') = do unify l l'; unify r r'
unify (MetaK kv1) (MetaK kv2) | kv1 == kv2 = return ()
unify (MetaK kv) kn = unifyVar kv kn
unify kn (MetaK kv) = unifyVar kv kn
unify _ _ = throw UnificationFail

unifyVar :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> m ()
unifyVar kv1 kn2 = do
    mb_kn1 <- readMetaKv kv1
    case (mb_kn1, kn2) of
        (Just kn1, _) -> unify kn1 kn2
        (Nothing, MetaK kv2) ->
            readMetaKv kv2 >>= \case
                Just kn2 -> unify (MetaK kv1) kn2
                Nothing -> writeMetaKv kv1 kn2
        (Nothing, _) -> do
            occursCheck kv1 kn2
            writeMetaKv kv1 kn2

occursCheck :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> m ()
occursCheck kv1 kn2 = do
    kvs2 <- getMetaKvs kn2
    when (kv1 `S.member` kvs2) $ throw OccursCheckFail
