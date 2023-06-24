module Plato.Driver.Pipeline where

import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.Text

import Plato.Driver.Flag
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.TypToCore
import Plato.Typing

compile :: FilePath -> Plato ()
compile src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        whenFlagOn FDumpParsed $ liftIO $ putDoc $ pretty pssyn'
        typsyn <- psToTyp pssyn'
        typsyn' <- typing typsyn
        whenFlagOn FDumpTyped $ liftIO $ putDoc $ pretty typsyn'
        coresyn <- typToCore typsyn'
        whenFlagOn FDumpCore $ liftIO $ putDoc $ pretty coresyn
        return ()