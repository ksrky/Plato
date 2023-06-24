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
nicify tdecs =
        runReaderT (nicifyDecls tdecs) initFixityEnv
                `catches` [ Handler $ \e@LocErr{} -> liftIO (print e) >> return []
                          , Handler $ \e@PlainErr{} -> liftIO (print e) >> return []
                          , Handler $ \(e :: SomeException) -> liftIO (print e) >> return []
                          ]

nicifyDecls :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => [LTopDecl] -> m [LTopDecl]
nicifyDecls decs = do
        decs' <- opParse decs
        let (tds, lds) = groupingDecl decs'
        return $ tds ++ lds

groupingDecl :: [LTopDecl] -> ([LTopDecl], [LDecl])
groupingDecl decs = execWriter $ forM decs $ \dec -> case dec of
        L _ DataD{} -> tell ([dec], [])
        _ -> tell ([], [dec])
