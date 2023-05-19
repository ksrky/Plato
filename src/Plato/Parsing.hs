module Plato.Parsing (parseFile) where

import Control.Monad.IO.Class
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