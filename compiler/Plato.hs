module Plato (runPlato) where

import Control.Monad.IO.Class

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp

-- import Plato.RunCore
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> IO ()
runPlato src = catchError $ unPlato (compile src) =<< initSession

compile :: FilePath -> Plato ()
compile src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        coresyn <- typ2core typsyn'
        liftIO $ print coresyn