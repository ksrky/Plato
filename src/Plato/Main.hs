{-# LANGUAGE OverloadedStrings #-}

module Plato.Main where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc

import Plato.Main.Monad

import Plato.Transl.CoreToIO
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline
import System.Directory
import System.FilePath (takeDirectory, (</>))

runPlato :: FilePath -> IO ()
runPlato src = catchError $ evalStateT (processFile src) initPlatoState{basePath = takeDirectory src}

repl :: [FilePath] -> IO ()
repl files = do
        base_path <- getCurrentDirectory
        st <- execStateT (mapM processFile files) initPlatoState{isEntry = False, basePath = base_path}
        runInputT defaultSettings (catchError $ loop st{isEntry = True})
    where
        loop :: (MonadIO m, MonadMask m) => PlatoState -> InputT m ()
        loop st = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just input -> do
                                st' <- continueError (return st) $ process (T.pack input) `execStateT` st
                                loop st'

processFile :: (MonadThrow m, MonadIO m) => FilePath -> Plato m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        is_entry <- gets isEntry
        -- processing Parsing
        optab <- gets opTable
        (optab', ps) <- src2ps optab input
        -- processing Imports
        -- let mbmod = moduleName ps
        --     imps = importModule ps
        -- mapM_ (processImport mbmod) imps
        -- processing Typing
        st <- get
        (typ, typenv') <- ps2typ (typingEnv st) ps
        -- processing Core
        let ctx = context st
        (ns', modul) <- typ2core (renames st) ctx typ
        ctx' <- processModule is_entry ctx modul
        put st{opTable = optab', typingEnv = typenv', renames = ns', context = ctx'}

{-}
processImport :: (MonadThrow m, MonadIO m) => Maybe ModuleName -> Located ModuleName -> Plato m ()
processImport mbmod (L sp impmod) = do
        is_entry <- gets isEntry
        imped_list <- gets importedList
        impng_list <- gets importingList
        when (impmod `elem` impng_list) $ throwLocErr sp "Cyclic dependencies"
        unless (impmod `elem` imped_list) $ do
                modify $ \s -> s{isEntry = False, importingList = impmod : impng_list}
                path <- solveModpath mbmod impmod
                base_path <- gets basePath
                processFile (base_path </> mod2path impmod)
                modify $ \s -> s{isEntry = is_entry, importedList = impmod : importedList s, importingList = impng_list}
-}