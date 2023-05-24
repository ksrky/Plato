{-# LANGUAGE FlexibleInstances #-}

module Plato.Typing.Env (
        TypEnv,
        HasTypEnv (..),
        EnvManager (..),
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M

import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Syntax.Typing

type TypEnv = M.Map GlbName Sigma

class HasTypEnv a where
        getEnv :: Monad m => a -> m TypEnv
        modifyEnv :: (TypEnv -> TypEnv) -> a -> a

instance HasTypEnv TypEnv where
        getEnv = return
        modifyEnv = id

class EnvManager a where
        extend :: Name -> a -> TypEnv -> TypEnv
        find :: MonadThrow m => Name -> TypEnv -> m a