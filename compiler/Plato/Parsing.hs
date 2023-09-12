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
import Plato.Parsing.OpParser
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseFile :: PlatoMonad m => FilePath -> m Program
parseFile src = catchPsErrors $ do
        inp <-
                try (liftIO $ T.readFile src) >>= \case
                        Left (_ :: SomeException) -> throwError $ pretty src <> ": file does not exist."
                        Right inp -> return inp
        uref <- getUniq =<< ask
        (prog, _) <- liftIO $ parse src uref inp parser
        updateContext $ opParseTop prog

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