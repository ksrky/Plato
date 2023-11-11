module Plato.Driver.Context (Context, initContext) where


import Data.IORef

import Plato.Common.Uniq
import Plato.Core.Env
import Plato.PsToTyp.Scoping
import Plato.Typing.Env

data Context = Context
    { ctx_uniq    :: !(IORef Uniq)
    , ctx_scope   :: !Scope
    , ctx_typEnv  :: !TypEnv
    , ctx_coreEnv :: !CoreEnv
    }

initContext :: IO Context
initContext = do
    uref <- initUniq
    ceref <- initCoreEnv
    return $ Context
                    { ctx_uniq = uref
                    , ctx_scope = mempty
                    , ctx_typEnv = mempty
                    , ctx_coreEnv = ceref
                    }

instance HasUniq Context where
    getUniq = return . ctx_uniq

instance HasScope Context where
    getScope =  ctx_scope
    setScope sc ctx = ctx{ctx_scope = sc}

instance HasTypEnv Context where
    getTypEnv = ctx_typEnv
    setTypEnv env ctx = ctx{ctx_typEnv = env}

instance HasCoreEnv Context where
    getCoreEnv = getCoreEnv . ctx_coreEnv
