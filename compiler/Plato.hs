module Plato (
        runPlato,
        module Plato.Driver.Monad,
) where

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato = unPlato . compile

compile :: FilePath -> Plato ()
compile src = catchError $ do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        _coresyn <- typ2core typsyn'
        return ()