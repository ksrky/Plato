module Plato.Parsing (
        parseFile,
        parsePartial,
        parseExpr,
        parseDecls,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T

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
        tdecs <- unlessImported filename parseFile
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')
processInstrs (L _ (TopDecls tdecs) : rest) = do
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')

parsePartial :: (MonadReader env m, HasUniq env, MonadIO m) => Parser a -> T.Text -> m a
parsePartial parser inp = do
        uref <- getUniq =<< ask
        (ast, _) <- liftIO $ parseLine uref inp parser
        return ast

parseExpr :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> m LExpr
parseExpr = parsePartial exprParser

parseDecls :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> m [LDecl]
parseDecls = parsePartial declsParser