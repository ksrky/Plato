{-# LANGUAGE TupleSections #-}

module Plato.KindCheck.Utils where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Plato.Common.Error
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type
import Plato.Typing.Monad
import Plato.Typing.Zonking

getMetaKvs :: MonadIO m => Kind -> Typ m (S.Set MetaKv)
getMetaKvs kn = do
        kn' <- zonkKind kn
        return (metaKvs kn')

metaKvs :: Kind -> S.Set MetaKv
metaKvs StarK = S.empty
metaKvs (ArrK kn1 kn2) = metaKvs kn1 `S.union` metaKvs kn2
metaKvs (MetaK kv) = S.singleton kv
