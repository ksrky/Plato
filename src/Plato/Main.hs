module Plato.Main where

import Plato.Transl.CoreToIO
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Monad
import Plato.Types.Name

import Control.Exception.Safe
import Control.Monad.RWS
import qualified Data.Text as T
import qualified Data.Text.IO as T

runPlato :: FilePath -> IO ()
runPlato src = catchError $ fst <$> evalRWST (processFile src) initPInfo initPState

processFile :: (MonadThrow m, MonadIO m) => FilePath -> Plato m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        (fixenv, ps) <- src2ps input
        (imp_modns, store) <- listen $ mapM processImport (importModules ps)
        ps' <- psCanon fixenv ps
        typ <- ps2typ ps'
        --mod <- typ2core typ
        liftIO $ print typ

processImport :: Located ModuleName -> Plato m [ModuleName]
processImport = undefined