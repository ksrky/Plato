module Plato.Driver.Context where

import Control.Monad.IO.Class
import Data.IORef

import Plato.Common.Uniq
import Plato.Interpreter.Core
import Plato.Nicifier.OpParser
import Plato.PsToTyp.Scoping
import Plato.Typing.Env

data Context = Context
        { ctx_uniq :: IORef Uniq
        , ctx_fixityEnv :: FixityEnv
        , ctx_scope :: Scope
        , ctx_typEnv :: TypEnv
        , ctx_conEnv :: ConEnv
        , ctx_coreEnv :: CoreEnv
        }

initContext :: IO Context
initContext = do
        uref <- initUniq
        coreenv <- initCoreEnv
        return $
                Context
                        { ctx_uniq = uref
                        , ctx_fixityEnv = initFixityEnv
                        , ctx_scope = initScope
                        , ctx_typEnv = initTypEnv
                        , ctx_conEnv = initConEnv
                        , ctx_coreEnv = coreenv
                        }

instance HasUniq Context where
        getUniq = return . ctx_uniq
        setUniq uniq ctx = liftIO $ writeIORef (ctx_uniq ctx) uniq

instance HasFixityEnv Context where
        getFixityEnv = ctx_fixityEnv
        modifyFixityEnv f ctx = ctx{ctx_fixityEnv = f (ctx_fixityEnv ctx)}

instance HasScope Context where
        getScope = ctx_scope
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

instance HasTypEnv Context where
        getTypEnv = ctx_typEnv
        modifyTypEnv f ctx = ctx{ctx_typEnv = f (ctx_typEnv ctx)}

instance HasConEnv Context where
        getConEnv = ctx_conEnv
        modifyConEnv f ctx = ctx{ctx_conEnv = f (ctx_conEnv ctx)}

instance HasCoreEnv Context where
        getCoreEnv = ctx_coreEnv
        modifyCoreEnv f ctx = ctx{ctx_coreEnv = f (ctx_coreEnv ctx)}