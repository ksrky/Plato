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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline

runPlato :: FilePath -> IO ()
runPlato src = catchError $ fst <$> evalRWST (processFile src) initPInfo initPState

repl :: [FilePath] -> IO ()
repl files = do
        (_, st, _) <- runRWST (mapM processFile files) initPInfo{plt_isEntry = False} initPState
        runInputT defaultSettings (catchError $ fst <$> evalRWST loop initPInfo st)
    where
        loop :: (MonadIO m, MonadMask m) => Plato (InputT m) ()
        loop = do
                minput <- lift $ getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just input -> continueError (return ()) $ process (T.pack input)

processFile :: (MonadThrow m, MonadIO m) => FilePath -> Plato m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        (fixenv, ps) <- src2ps input
        (imp_modns, _) <- listen $ mapM processImport (importModules ps)
        ps' <- psCanon imp_modns fixenv ps
        typ <- ps2typ ps'
        mod <- typ2core typ
        processModule mod

processImport :: (MonadThrow m, MonadIO m) => Located ModuleName -> Plato m ModuleName
processImport (L sp modn) = do
        impng_set <- asks plt_importingList
        imped_set <- gets plt_importedList
        when (modn `S.member` impng_set) $ throwLocErr sp "Cyclic dependencies"
        unless (modn `S.member` imped_set) $ do
                local
                        (\r -> r{plt_isEntry = False, plt_importingList = modn `S.insert` impng_set})
                        $ processFile undefined
                modify $ \s -> s{plt_importedList = modn `S.insert` plt_importedList s}
        return modn