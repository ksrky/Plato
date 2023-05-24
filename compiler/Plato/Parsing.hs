module Plato.Parsing (parseFile, parsePartial) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Plato.Driver.Monad
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m Program
parseFile src = do
        inp <- liftIO $ T.readFile src
        (program, st) <- liftIO $ parse src inp parser
        setUniq $ ust_uniq (parser_ust st)
        return program

parsePartial :: MonadIO m => T.Text -> Parser a -> m a
parsePartial inp parser = do
        (ast, _) <- liftIO $ parseLine inp parser
        return ast