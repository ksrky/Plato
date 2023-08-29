module Plato (
        runPlato,
        compileToCore,
        evaluateCore,
        module Plato.Driver.Monad,
        module Plato.Driver.Interactive,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Text qualified as T

import Plato.Common.Error
import Plato.Common.Pretty
import Plato.Driver.Interactive
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.Syntax.Core
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato filepath = void . unPlato (compileToCore filepath)

compileToCore :: PlatoMonad m => FilePath -> m Prog
compileToCore src = catchErrors $ do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        whenFlagOn FPrintParsed $ liftIO $ prettyPrint pssyn'
        typsyn <- psToTyp pssyn'
        typsyn' <- typing typsyn
        whenFlagOn FPrintTyped $ liftIO $ prettyPrint typsyn'
        corsyn <- typToCore typsyn'
        whenFlagOn FPrintCore $ liftIO $ prettyPrint corsyn
        whenFlagOn FEvalCore $ appendProg corsyn
        return corsyn

evaluateCore :: PlatoMonad m => T.Text -> Interactive m ()
evaluateCore inp =
        catchErrors $
                lift
                        ( do
                                pssyn <- parseExpr inp
                                pssyn' <- nicifyExpr pssyn
                                typsyn <- psToTypExpr pssyn'
                                typsyn' <- typingExpr typsyn
                                typToCoreExpr typsyn'
                        )
                        >>= evalCore