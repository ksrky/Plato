module Plato.Driver.Interactive (Interactive, evalCore, printList, runInteractive) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Prettyprinter.Render.Text
import Prettyprinter.Util

import Plato.Common.Pretty
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Pretty
import Plato.Driver.Monad
import Plato.Syntax.Core

data InteractEnv = InteractEnv { int_scope :: Scope
                                 }

type Interactive m = ReaderT InteractEnv m

runInteractive ::
    (PlatoMonad m) =>
    Interactive m a -> m a
runInteractive m = do
    env <- readCoreEnv =<< getContext
    runReaderT m $ InteractEnv{int_scope = restoreScope env}

evalCore :: (PlatoMonad m) => Term -> Interactive m ()
evalCore t = do
    sc <- asks int_scope
    lift $ applyContext $ liftIO . putDoc =<< evalPrint =<< eval (t, sc)
    liftIO $ putStrLn ""

printList :: (Pretty a) => [a] -> IO ()
printList = putDocW 100 . (<> line) . vsep . map pretty
