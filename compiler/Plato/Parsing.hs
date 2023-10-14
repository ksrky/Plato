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

import Plato.Common.Error
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
        uref <- getUniq =<< ask
        (prog, _) <- liftIO $ parse src uref inp parser
        return prog

{- parseInstr :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m Instr
parseInstr inp = do
        uref <- getUniq =<< ask
        (instr, _) <- liftIO $ parseLine uref inp instrParser
        updateContext $ opParseInstr instr -}

parsePartial ::
        (MonadReader e m, HasUniq e, MonadIO m) =>
        Parser a ->
        T.Text ->
        m a
parsePartial parser inp = do
        uref <- getUniq =<< ask
        (a, _) <- liftIO $ parseLine uref inp parser
        return a

parseExpr :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m LExpr
parseExpr = catchPsErrors . parsePartial exprParser

parseDecls :: (MonadReader e m, HasUniq e, MonadIO m, MonadCatch m) => T.Text -> m [LTopDecl]
parseDecls = catchPsErrors . parsePartial declsParser