{-# LANGUAGE LambdaCase #-}

module Plato.Parsing (
        parseFile,
        parsePartial,
        parseExpr,
        parseDecls,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter

import {-# SOURCE #-} Plato (compileToCore)
import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Import
import Plato.Driver.Monad
import Plato.Parsing.Error
import Plato.Parsing.Monad
import Plato.Parsing.OpParser
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m [LTopDecl]
parseFile src = catchPsErrors $ do
        inp <-
                liftIO $
                        try (T.readFile src) >>= \case
                                Left (_ :: SomeException) -> throwError $ pretty src <> ": file does not exist."
                                Right inp -> return inp
        uref <- getUniq =<< ask
        (prog, _) <- liftIO $ parse src uref inp parser
        tdecs <- runReaderT (processInstrs prog) emptyImporting
        updateContext $ opParseTop tdecs

processInstrs :: PlatoMonad m => [LInstr] -> ReaderT Importing m [LTopDecl]
processInstrs [] = return []
processInstrs (L _ (ImpDecl filename) : rest) = do
        checkCyclicImport filename
        _ <- unlessImported filename compileToCore
        processInstrs rest
processInstrs (L _ (TopDecls tdecs) : rest) = do
        tdecs' <- processInstrs rest
        return (tdecs ++ tdecs')
processInstrs (L _ (EvalExpr _exp) : rest) = processInstrs rest

parsePartial ::
        (OpParser a, MonadReader e m, HasUniq e, HasFixityEnv e, MonadIO m, MonadThrow m) =>
        Parser a ->
        T.Text ->
        m a
parsePartial parser inp = do
        uref <- getUniq =<< ask
        (a, _) <- liftIO $ parseLine uref inp parser
        opParse a

parseExpr :: (MonadReader e m, HasUniq e, HasFixityEnv e, MonadIO m, MonadCatch m) => T.Text -> m LExpr
parseExpr = catchPsErrors . parsePartial exprParser

parseDecls :: (MonadReader e m, HasUniq e, HasFixityEnv e, MonadIO m, MonadCatch m) => T.Text -> m [LTopDecl]
parseDecls = catchPsErrors . parsePartial declsParser