module Plato.Parsing (parseFile, parsePartial) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Import
import Plato.Driver.Monad
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m [LTopDecl]
parseFile src = ( `catches`
                        [ Handler $ \e@LocErr{} -> liftIO (print e) >> return []
                        , Handler $ \e@PlainErr{} -> liftIO (print e) >> return []
                        , Handler $ \(e :: SomeException) -> liftIO (print e) >> return []
                        ]
                )
        $ do
                inp <- liftIO $ T.readFile src
                uref <- getUniq =<< ask
                (prog, _) <-
                        runReaderT (liftIO $ parse src uref inp parser) uref
                uniq <- readUniq uref
                setUniq uniq =<< ask
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

parsePartial :: (MonadReader env m, HasUniq env, MonadIO m) => T.Text -> Parser a -> m a
parsePartial inp parser = do
        uniq <- getUniq =<< ask
        (ast, _) <- liftIO $ parseLine uniq inp parser
        return ast