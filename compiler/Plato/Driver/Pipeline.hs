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
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        whenFlagOn FDumpTyped $ liftIO $ putDoc $ pretty typsyn'
        coresyn <- typ2core typsyn'
        whenFlagOn FDumpCore $ liftIO $ putDoc $ pretty coresyn
        return ()

-- coresyn' <- runCore coresyn
-- liftIO $ mapM_ printResult coresyn'

{-dynCompile :: MonadIO m => T.Text -> PlatoT m ()
dynCompile inp = do
        pssyn <- parsePartial inp exprParser
        pssyn' <- opParse pssyn
        typsyn <- elabExpr $ unLoc pssyn'
        typsyn' <- inferType typsyn
        coresyn <- typ2core typsyn'
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'-}