module Plato (
        runPlato,
        module Plato.Driver.Monad,
) where

import Control.Monad.IO.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato src session = catchError $ unPlato (compile src) session

compile :: FilePath -> Plato ()
compile src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        coresyn <- typ2core typsyn'
        liftIO $ print $ pretty coresyn