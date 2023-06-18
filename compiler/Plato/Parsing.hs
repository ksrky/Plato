module Plato.Parsing (parseFile, parsePartial) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Import
import Plato.Driver.Monad
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m [LTopDecl]
parseFile src = do
        inp <- liftIO $ T.readFile src
        uniq <- getUniq =<< ask
        (prog, _) <- runReaderT (liftIO $ parse src uniq inp parser) uniq
        setUniq uniq
        processInstrs prog

processInstrs :: PlatoMonad m => [LInstr] -> m [LTopDecl]
processInstrs [] = return []
processInstrs (L _ (ImpDecl filename) : rest) = do
        tdecs <- unlessImported filename parseFile
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')
processInstrs (L _ (TopDecls tdecs) : rest) = do
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')

parsePartial :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> Parser a -> m a
parsePartial inp parser = do
        uniq <- getUniq =<< ask
        (ast, _) <- liftIO $ parseLine uniq inp parser
        return ast