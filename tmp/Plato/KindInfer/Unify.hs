module Plato.KindInfer.Unify where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S
import Prettyprinter

import Plato.KindInfer.Monad
import Plato.KindInfer.Utils
import Plato.Syntax.Typing

unify :: (MonadThrow m, MonadIO m) => Kind -> Kind -> Ki m ()
unify StarK StarK = return ()
unify (ArrK l r) (ArrK l' r') = do
        unify l l'
        unify r r'
unify (MetaK kv1) (MetaK kv2) | kv1 == kv2 = return ()
unify (MetaK kv) kn = unifyVar kv kn
unify kn (MetaK kv) = unifyVar kv kn
unify kn1 kn2 = do
        sp <- readErrLoc
        throwKi sp $ vsep ["Couldn't match kind.", "Expected kind:" <+> pretty kn2, indent 2 ("Actual type:" <+> pretty kn1)]

unifyVar :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> Ki m ()
unifyVar kv1 kn2@(MetaK kv2) = do
        mb_kn1 <- readMetaKv kv1
        mb_kn2 <- readMetaKv kv2
        case (mb_kn1, mb_kn2) of
                (Just kn1, _) -> unify kn1 kn2
                (Nothing, Just kn2) -> unify (MetaK kv1) kn2
                (Nothing, Nothing) -> writeMetaKv kv1 kn2
unifyVar kv1 kn2 = do
        occursCheck kv1 kn2
        writeMetaKv kv1 kn2

unifyFun :: (MonadIO m, MonadThrow m) => Kind -> Ki m (Kind, Kind)
unifyFun (ArrK kn1 kn2) = return (kn1, kn2)
unifyFun kn = do
        arg_kn <- newKnVar
        res_kn <- newKnVar
        unify kn (ArrK arg_kn res_kn)
        return (arg_kn, res_kn)

occursCheck :: (MonadThrow m, MonadIO m) => MetaKv -> Kind -> Ki m ()
occursCheck kv1 kn2 = do
        kvs2 <- getMetaKvs kn2
        when (kv1 `S.member` kvs2) $ do
                sp <- readErrLoc
                throwKi sp $ hsep ["Infinite kind:", squotes $ pretty kn2]