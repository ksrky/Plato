module Plato.Interpreter where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.IORef
import Prettyprinter
import Prettyprinter.Render.Text

import Plato.Common.Uniq
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Pretty
import Plato.Driver.Context
import Plato.Driver.Monad
import Plato.Interpreter.Core
import Plato.Syntax.Core

addCoreEnv :: PlatoMonad m => Prog -> m ()
addCoreEnv prog = do
        ctx <- getContext =<< ask
        sc <- runReaderT (enterCore prog) (getCoreEnv ctx)
        let CoreEnv env _ = getCoreEnv ctx
        setContext (setCoreEnv (CoreEnv env sc) ctx) =<< ask

appendProg :: PlatoMonad m => Prog -> m ()
appendProg prog = do
        ctx <- getContext =<< ask
        setContext (ctx{ctx_coreProg = ctx_coreProg ctx ++ prog}) =<< ask

data InteractEnv = InteractEnv
        { int_envent :: IORef EnvEntries
        , int_scope :: CoreScope
        , int_uniq :: IORef Uniq
        }

type Interactive m = ReaderT InteractEnv m

getEnvEntries :: MonadIO m => Interactive m (IORef EnvEntries)
getEnvEntries = asks int_envent

getCoreScope :: Monad m => Interactive m CoreScope
getCoreScope = asks int_scope

instance HasUniq InteractEnv where
        getUniq (InteractEnv _ _ uref) = return uref
        setUniq uniq (InteractEnv _ _ uref) = setUniq uniq uref

instance Env InteractEnv where
        extE fi = extE fi . int_envent
        getE i = getE i . int_envent
        setE i e = setE i e . int_envent
        prtE i = prtE i . int_envent

runInteractive ::
        PlatoMonad m =>
        Interactive m a ->
        m a
runInteractive m = do
        eref <- liftIO $ newIORef mempty
        prog <- ctx_coreProg <$> (getContext =<< ask)
        sc <- runReaderT (evalProg (prog, emptyScope)) eref
        uref <- getUniq =<< ask
        runReaderT m $
                InteractEnv
                        { int_envent = eref
                        , int_scope = sc
                        , int_uniq = uref
                        }

evalCore :: (MonadThrow m, MonadIO m) => Term -> Interactive m ()
evalCore t = do
        sc <- getCoreScope
        doc <- evalPrint =<< eval (t, sc)
        liftIO $ putDoc $ doc <> line
