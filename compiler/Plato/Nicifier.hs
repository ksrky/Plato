module Plato.Nicifier (nicify, nicifyExpr) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Plato.Common.Error
import Plato.Common.Location
import Plato.Driver.Monad
import Plato.Nicifier.OpParser
import Plato.Syntax.Parsing

-- TODO: detect mutual recursion for both data types and functions
nicify :: PlatoMonad m => [LTopDecl] -> m [LTopDecl]
nicify tdecs = catchErrors $ updateContext (nicifyDecls tdecs)

nicifyDecls :: (MonadReader e m, HasFixityEnv e, MonadThrow m) => [LTopDecl] -> m ([LTopDecl], e)
nicifyDecls decs = do
        fixenv <- asks getFixityEnv
        (decs', fixenv') <- runReaderT (opParseTop decs) fixenv
        env <- ask
        return (decs', setFixityEnv fixenv' env)

nicifyExpr :: PlatoMonad m => LExpr -> m LExpr
nicifyExpr exp = do
        fixenv <- getFixityEnv <$> (getContext =<< ask)
        runReaderT (opParse exp) fixenv
