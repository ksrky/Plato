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
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline
import System.Directory
import System.FilePath (takeDirectory, (</>))

runPlato :: FilePath -> IO ()
runPlato src = catchError $ fst <$> evalPlatoM (processFile src) initPInfo initPState

repl :: [FilePath] -> IO ()
repl files = undefined {-do
                           base_path <- getCurrentDirectory
                           (_, ps, pw) <- runPlatoM (mapM processFile files) initPR initPS
                           runInputT defaultSettings (catchError undefined)
                       where
                           loop :: (MonadIO m, MonadMask m) => InputT (PlatoM m) ()
                           loop = do
                                   minput <- getInputLine ">> "
                                   case minput of
                                           Nothing -> return ()
                                           Just "" -> return ()
                                           Just input -> do
                                                   pr <- RWS.ask
                                                   ps <- RWS.get
                                                   undefined-}

-- st' <- continueError (return st) $ runPlatoM (process (T.pack input)) pr ps
-- loop

processFile :: (MonadThrow m, MonadIO m) => FilePath -> PlatoM m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input

process :: (MonadThrow m, MonadIO m) => T.Text -> PlatoM m ()
process input = do
        is_entry <- asks pr_isEntry
        -- processing Parsing
        optab <- gets ps_opTable
        (optab', ps) <- src2ps optab input
        -- processing Imports
        modn <- case moduleName ps of
                Just modn' -> return modn'
                Nothing
                        | is_entry -> undefined
                        | otherwise -> throwError "Module name declaration missing"
        let imps = importModule ps
        (_, store) <- listen $ mapM processImport imps
        -- processing Typing
        st <- get
        (typ, typenv') <- ps2typ (ps_typTable st) ps
        -- processing Core
        let ctx = ps_context st
        (names', modul) <- typ2core (pw_nameTable store) ctx typ
        ctx' <- processModule is_entry ctx modul
        put st{ps_opTable = optab', ps_typTable = typenv', ps_context = ctx'}
        tell PStore{pw_nameTable = M.singleton modn names'}

processImport :: (MonadThrow m, MonadIO m) => Located ModuleName -> PlatoM m ()
processImport (L sp impmod) = do
        imped_set <- gets ps_importedSet
        impng_set <- asks pr_importingSet
        when (impmod `S.member` impng_set) $ throwLocErr sp "Cyclic dependencies"
        if impmod `S.member` imped_set
                then undefined -- tmp: add external nametable to writer
                else local (\r -> r{pr_isEntry = False, pr_importingSet = impmod `S.insert` imped_set}) $ do
                        let path = mod2path impmod --tmp
                        processFile path
