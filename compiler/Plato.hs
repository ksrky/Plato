module Plato (
        runPlato,
        compileToCore,
        evaluateCore,
        module Plato.Driver.Monad,
        module Plato.Driver.Interactive,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Driver.Interactive
import Plato.Driver.Monad
import Plato.Parsing
import Plato.PsToTyp as PT
import Plato.Syntax.Core
import Plato.TypToCore as TC
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato filepath = void . unPlato (compileToCore filepath)

compileToCore :: PlatoMonad m => FilePath -> m Prog
compileToCore src = catchErrors $ do
        pssyn <- parseFile src
        whenFlagOn FPrintParsed $ liftIO $ printList pssyn
        typsyn <- psToTyp pssyn
        typsyn' <- typing typsyn
        whenFlagOn FPrintTyped $ liftIO $ printList typsyn'
        corsyn <- typToCore typsyn'
        whenFlagOn FPrintCore $ liftIO $ printList corsyn
        whenFlagOn FEvalCore $ appendProg corsyn
        return corsyn

evaluateCore :: PlatoMonad m => T.Text -> Interactive m ()
evaluateCore inp =
        catchErrors $
                lift
                        ( runReaderT
                                ( do
                                        pssyn <- parseExpr inp
                                        typsyn <- PT.elabExpr `traverse` pssyn
                                        typsyn' <- typingExpr typsyn
                                        TC.elabExpr (unLoc typsyn')
                                )
                                =<< getContext
                                =<< ask
                        )
                        >>= evalCore