module Plato (
        runPlato,
        compileToCore,
        module Plato.Driver.Monad,
        module Plato.Interpreter,
) where

import Plato.Driver.Monad
import Plato.Interpreter
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> Session -> IO ()
runPlato = unPlato . compileToCore

compileToCore :: FilePath -> Plato ()
compileToCore src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        _coresyn <- typ2core typsyn'
        return ()