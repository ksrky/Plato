module Plato.Interpreter.Core (
        CoreEnv (..),
        initCoreEnv,
        HasCoreEnv (..),
        enterCore,
        runCore,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Prettyprinter
import Prettyprinter.Render.Text

import Plato.Common.Uniq
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Pretty
import Plato.Syntax.Core

data CoreEnv = CoreEnv (IORef EnvEntries) CoreScope

initCoreEnv :: IO CoreEnv
initCoreEnv = do
        envref <- newIORef []
        return $ CoreEnv envref emptyScope

instance Env CoreEnv where
        extE fi (CoreEnv env _) = extE fi env
        getE i (CoreEnv env _) = getE i env
        setE i v (CoreEnv env _) = setE i v env
        prtE i (CoreEnv env _) = prtE i env

class HasCoreEnv e where
        getCoreEnv :: e -> CoreEnv
        modifyCoreEnv :: (CoreEnv -> CoreEnv) -> e -> e
        setCoreEnv :: CoreEnv -> e -> e
        setCoreEnv = modifyCoreEnv . const

instance HasCoreEnv CoreEnv where
        getCoreEnv = id
        modifyCoreEnv = id

enterCore :: (MonadReader e m, HasCoreEnv e, MonadThrow m, MonadIO m) => Prog -> m CoreScope
enterCore prog = do
        CoreEnv env sc <- asks getCoreEnv
        runReaderT (evalProg (prog, sc)) env

data Context = Context (IORef EnvEntries) (IORef Uniq)

instance HasUniq Context where
        getUniq (Context _ uref) = getUniq uref
        setUniq uniq (Context _ uref) = setUniq uniq uref

instance Env Context where
        extE fi (Context env _) = extE fi env
        getE i (Context env _) = getE i env
        setE i v (Context env _) = setE i v env
        prtE i (Context env _) = prtE i env

runCore :: (MonadReader e m, HasCoreEnv e, MonadThrow m, MonadIO m) => IORef Uniq -> Term -> m ()
runCore uref t = do
        CoreEnv env sc <- asks getCoreEnv
        doc <- runReaderT (evalPrint =<< eval (t, sc)) (Context env uref)
        liftIO $ putDoc $ doc <> line