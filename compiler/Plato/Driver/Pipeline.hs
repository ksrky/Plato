module Plato.Driver.Pipeline where

import Control.Monad.IO.Class

import Plato.Driver.Monad
import Plato.Parsing
import Plato.PsToTyp
import Plato.RunCore
import Plato.Scoping
import Plato.TypToCore
import Plato.Typing

process :: FilePath -> Plato ()
process src = do
        pssyn <- parseFile src
        isFlagOn "ddump-parsing" $ liftIO $ print pssyn
        pssyn' <- scopingProgram pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typingProgram typsyn
        coresyn <- typ2core typsyn'
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'