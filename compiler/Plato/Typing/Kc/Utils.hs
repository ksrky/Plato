module Plato.Typing.Kc.Utils (getMetaKvs) where

import Control.Monad.IO.Class (MonadIO)
import Data.Set qualified as S

import Plato.Syntax.Typing
import Plato.Typing.Zonking

getMetaKvs :: MonadIO m => Kind -> m (S.Set MetaKv)
getMetaKvs kn = metaKvs <$> zonk kn

metaKvs :: Kind -> S.Set MetaKv
metaKvs StarK = S.empty
metaKvs (ArrK kn1 kn2) = metaKvs kn1 `S.union` metaKvs kn2
metaKvs (MetaK kv) = S.singleton kv