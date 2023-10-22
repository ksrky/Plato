module Plato (
    runPlato,
    compileToCore,
    evaluateCore,
    module Plato.Driver.Flag,
    module Plato.Driver.Interactive,
    module Plato.Driver.Monad,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text                qualified as T

import Plato.Common.Error
import Plato.Driver.Context
import Plato.Driver.Flag
import Plato.Driver.Interactive
import Plato.Driver.Monad
import Plato.Parsing
import Plato.PsToTyp
import Plato.Syntax.Core
import Plato.Typing
import Plato.TypToCore

runPlato :: FilePath -> Session -> IO ()
runPlato filepath = void . unPlato (compileToCore filepath)

compileToCore :: (PlatoMonad m) => FilePath -> m Prog
compileToCore src = catchErrors $ do
    pssyn <- parseFile src
    whenFlagOn FPrintParsed $ liftIO $ printList pssyn
    typsyn <- psToTyp pssyn
    typsyn' <- typing typsyn
    whenFlagOn FPrintTyped $ liftIO $ printList typsyn'
    corsyn <- typToCore typsyn'
    whenFlagOn FPrintCore $ liftIO $ printList corsyn
    return corsyn

evaluateCore :: forall m. (PlatoMonad m) => T.Text -> Interactive m ()
evaluateCore inp = catchErrors $ evalCore =<< lift (compExpr =<< getContext)
  where
    compExpr :: Context -> m Term
    compExpr = runReaderT $ parseExpr inp >>= psToTypExpr >>= typingExpr >>= typToCoreExpr
