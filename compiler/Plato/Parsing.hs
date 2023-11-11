{-# LANGUAGE LambdaCase #-}

module Plato.Parsing (parseCommand, parseDecls, parseExpr, parseFile, parsePartial) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text              qualified as T
import Data.Text.IO           qualified as T

import Plato.Common.Error
import Plato.Common.Pretty
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Parsing.Error
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

openFile :: (MonadCatch m, MonadIO m) => FilePath -> m T.Text
openFile src =
    try (liftIO $ T.readFile src) >>= \case
        Left (_ :: SomeException) -> throwError $ pretty src <> ": file does not exist."
        Right inp -> return inp

parseFile :: (PlatoMonad m) => FilePath -> m Program
parseFile src = catchPsErrors $ do
    inp <- openFile src
    uref <- getUniq =<< getContext
    (prog, _) <- liftIO $ parse src uref inp parser
    return prog

parsePartial :: (MonadReader e m, HasUniq e, MonadIO m) => Parser a -> T.Text -> m a
parsePartial parser inp = do
    uref <- getUniq =<< ask
    liftIO $ fst <$> parseLine uref inp parser

parseExpr :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m LExpr
parseExpr = catchPsErrors . parsePartial exprParser

parseDecls :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m [LTopDecl]
parseDecls = catchPsErrors . parsePartial declsParser

parseCommand :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m Command
parseCommand = catchPsErrors . parsePartial commandParser
