module Plato.Parsing (
        parseFile,
        parsePartial,
        parseInstr,
        parseExpr,
        parseDecls,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T

import {-# SOURCE #-} Plato (compileToCore)
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Import
import Plato.Driver.Monad
import Plato.Parsing.Error
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m [LTopDecl]
parseFile src = catchPsErrors $ do
        inp <- liftIO $ T.readFile src
        uref <- getUniq =<< ask
        (prog, _) <- runReaderT (liftIO $ parse src uref inp parser) uref
        runReaderT (processInstrs prog) emptyImporting

processInstrs :: PlatoMonad m => [LInstr] -> ReaderT Importing m [LTopDecl]
processInstrs [] = return []
processInstrs (L _ (ImpDecl filename) : rest) = do
        checkCyclicImport filename
        unlessImported filename compileToCore
        processInstrs rest
processInstrs (L _ (TopDecls tdecs) : rest) = do
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')
processInstrs (L _ (EvalExpr _exp) : rest) = do
        processInstrs rest

parsePartial :: (MonadReader env m, HasUniq env, MonadIO m) => Parser a -> T.Text -> m a
parsePartial parser inp = do
        uref <- getUniq =<< ask
        (ast, _) <- liftIO $ parseLine uref inp parser
        return ast

parseInstr :: PlatoMonad m => T.Text -> m (Either [LTopDecl] LExpr)
parseInstr inp = do
        uref <- getUniq =<< ask
        instr <- runReaderT (parsePartial instrParser inp) uref
        runReaderT (processInstr instr) emptyImporting

processInstr :: PlatoMonad m => LInstr -> ReaderT Importing m (Either [LTopDecl] LExpr)
processInstr (L _ (ImpDecl filename)) = do
        checkCyclicImport filename
        unlessImported filename compileToCore
        return $ Left []
processInstr (L _ (EvalExpr exp)) = return $ Right exp
processInstr (L _ TopDecls{}) = return $ Left []

parseExpr :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> m LExpr
parseExpr = parsePartial exprParser

parseDecls :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> m [LDecl]
parseDecls = parsePartial declsParser