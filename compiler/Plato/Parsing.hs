module Plato.Parsing (parseFile, parsePartial) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m Program
parseFile src = do
        inp <- liftIO $ T.readFile src
        uniq <- getUniq =<< ask
        (program, _) <- runReaderT (liftIO $ parse src uniq inp parser) uniq
        setUniq uniq
        return program

parsePartial :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> Parser a -> m a
parsePartial inp parser = do
        uniq <- getUniq =<< ask
        (ast, _) <- liftIO $ parseLine uniq inp parser
        return ast