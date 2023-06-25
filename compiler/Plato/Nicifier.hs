{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
nicify tdecs = do
        ctx <- getContext =<< ask
        (res, fixenv') <- catchErrors $ nicifyDecls tdecs (getFixityEnv ctx)
        setContext (setFixityEnv fixenv' ctx) =<< ask
        return res

nicifyDecls :: MonadThrow m => [LDecl] -> FixityEnv -> m ([LDecl], FixityEnv)
nicifyDecls decs env = do
        let fixenv = getFixityEnv env
        (decs', fixenv') <- runReaderT (opParse (decs, fixenv)) fixenv
        let (tds, lds) = groupingDecl decs'
        return (tds ++ lds, fixenv')

groupingDecl :: [LDecl] -> ([LDecl], [LDecl])
groupingDecl decs = execWriter $ forM decs $ \dec -> case dec of
        L _ DataD{} -> tell ([dec], [])
        _ -> tell ([], [dec])

nicifyExpr :: PlatoMonad m => LExpr -> m LExpr
nicifyExpr exp = do
        fixenv <- getFixityEnv <$> (getContext =<< ask)
        runReaderT (opParse exp) fixenv