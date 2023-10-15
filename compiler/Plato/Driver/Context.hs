module Plato.Driver.Context where

import Control.Monad.IO.Class
import Data.IORef

import Plato.Common.Uniq
import Plato.PsToTyp.Scoping
import Plato.Syntax.Core
import Plato.Typing.Env

data Context = Context
        { ctx_uniq :: !(IORef Uniq)
        , ctx_scope :: !Scope
        , ctx_typEnv :: !TypEnv
        , ctx_conEnv :: !ConEnv
        , ctx_coreProg :: !Prog
        }

initContext :: IO Context
initContext = do
        uref <- initUniq
        return $
                Context
                        { ctx_uniq = uref
                        , ctx_scope = mempty
                        , ctx_typEnv = mempty
                        , ctx_conEnv = mempty
                        , ctx_coreProg = mempty
                        }

instance HasUniq Context where
        getUniq = return . ctx_uniq
        setUniq uniq ctx = liftIO $ writeIORef (ctx_uniq ctx) uniq

instance HasScope Context where
        getScope = ctx_scope
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

instance HasTypEnv Context where
        getTypEnv = ctx_typEnv
        modifyTypEnv f ctx = ctx{ctx_typEnv = f (ctx_typEnv ctx)}

instance HasConEnv Context where
        getConEnv = ctx_conEnv
        modifyConEnv f ctx = ctx{ctx_conEnv = f (ctx_conEnv ctx)}