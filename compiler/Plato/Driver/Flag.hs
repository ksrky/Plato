module Plato.Driver.Flag where

import Control.Monad

import Plato.Common.Error
import Plato.Driver.Monad

class HasFlags a where
        getFlags :: a -> [(String, Bool)]

instance HasFlags PlatoEnv where
        getFlags = plt_flags

setFlag :: PlatoMonad m => String -> Bool -> m ()
setFlag flag val = do
        env <- getSession
        setSession env{plt_flags = (flag, val) : plt_flags env}

isFlagOn :: PlatoMonad m => String -> m a -> m ()
isFlagOn flag action = do
        env <- getSession
        case lookup flag (getFlags env) of
                Just True -> void action
                Just False -> return ()
                Nothing -> unreachable $ "flag not found '" ++ flag ++ "'"