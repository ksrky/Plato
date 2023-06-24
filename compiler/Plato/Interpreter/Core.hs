module Plato.Interpreter.Core where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader

import Plato.Core.Data
import Plato.Core.Eval
import Plato.Interpreter
import Plato.Syntax.Core

data Env = Env EnvEntries Scope

instance CoreEnv Env

instance Interpreter (Env, Scope) where
        enter = undefined
        interp (env, sc) t = do
                val <- runReaderT (eval (t, sc)) env
                liftIO $ print val

runCore :: (CoreEnv e, MonadThrow m, MonadIO m) => e -> Scope -> Term -> m Val
runCore env sc t = runReaderT (eval (t, sc)) env