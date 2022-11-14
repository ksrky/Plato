module Plato.Main where

{-}
import Plato.Transl.SrcToPs
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
        is_entry <- asks plt_isEntry
        (fixenv, prg) <- src2ps input
        (_, store) <- listen $ mapM processImport (importModules prg)
        prg' <- canonical fixenv prg
        undefined

processImport :: (MonadThrow m, MonadIO m) => Located ModuleName -> Plato m ()
processImport = undefined-}