{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Plato.Nicifier (nicify) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Plato.Common.Error
import Plato.Common.Location
import Plato.Driver.Monad
import Plato.Nicifier.OpParser
import Plato.Nicifier.OpParser.Fixity
import Plato.Syntax.Parsing

-- TODO: detect mutual recursion for both data types and functions
nicify :: PlatoMonad m => [LTopDecl] -> m [LTopDecl]
nicify tdecs = catchErrors $ nicifyDecls tdecs initFixityEnv

nicifyDecls :: (HasFixityEnv e, MonadThrow m) => [LDecl] -> e -> m [LDecl]
nicifyDecls decs env = do
        decs' <- runReaderT (opParse decs) (getFixityEnv env)
        let (tds, lds) = groupingDecl decs'
        return $ tds ++ lds

groupingDecl :: [LDecl] -> ([LDecl], [LDecl])
groupingDecl decs = execWriter $ forM decs $ \dec -> case dec of
        L _ DataD{} -> tell ([dec], [])
        _ -> tell ([], [dec])
