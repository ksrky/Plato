module Plato.Driver.Pipeline where

import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.Text

import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.RunCore
import Plato.TypToCore
import Plato.Typing

process :: FilePath -> Plato ()
process src = do
        pssyn <- parseFile src
        isFlagOn "ddump-parsing" $ liftIO $ putDoc $ pretty pssyn
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        coresyn <- typ2core typsyn'
        isFlagOn "ddump-core" $ liftIO $ putDoc $ prettyCommands coresyn
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'

{-dynCompile :: MonadIO m => T.Text -> PlatoT m ()
dynCompile inp = do
        pssyn <- parsePartial inp exprParser
        pssyn' <- opParse pssyn
        typsyn <- elabExpr $ unLoc pssyn'
        typsyn' <- inferType typsyn
        coresyn <- typ2core typsyn'
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'-}