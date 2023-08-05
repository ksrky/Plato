module Plato (
        runPlato,
        compileToCore,
        interpretExpr,
        module Plato.Driver.Monad,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T

import Plato.Common.Error
import Plato.Common.Pretty
import Plato.Driver.Monad
import Plato.Interpreter
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato = unPlato . compileToCore

compileToCore :: PlatoMonad m => FilePath -> m ()
compileToCore src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        whenFlagOn FPrintParsed $ liftIO $ prettyPrint pssyn'
        typsyn <- psToTyp pssyn'
        typsyn' <- typing typsyn
        whenFlagOn FPrintTyped $ liftIO $ prettyPrint typsyn'
        coresyn <- typToCore typsyn'
        whenFlagOn FPrintCore $ liftIO $ prettyPrint coresyn
        whenFlagOn FEvalCore $ addCoreEnv coresyn

interpretExpr :: PlatoMonad m => T.Text -> m ()
interpretExpr inp = catchErrors $ do
        pssyn <- parseExpr inp
        pssyn' <- nicifyExpr pssyn
        typsyn <- psToTypExpr pssyn'
        typsyn' <- typingExpr typsyn
        coresyn <- typToCoreExpr typsyn'
        evalCore coresyn