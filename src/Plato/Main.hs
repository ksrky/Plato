{-# LANGUAGE OverloadedStrings #-}

module Plato.Main where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Main.Monad
import Plato.Syntax.Core
import qualified Plato.Syntax.Parsing as P
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Console.Haskeline
import System.FilePath ((</>))

runPlato :: FilePath -> IO ()
runPlato src = catchError $ evalStateT (processFile src) (initPlatoState src)

repl :: [FilePath] -> IO ()
repl files = do
        st <- execStateT (mapM processFile files) (initPlatoState ""){isEntry = False}
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
        st <- get
        -- processing Parsing
        ps <- src2ps input
        -- processing Imports
        let imps = P.importDecls ps
        mapM_ processModule imps
        -- processing Typing
        (typ, typenv') <- ps2typ (typingEnv st) ps
        -- processing Core
        let ctx = context st
        (ns', cmds) <- typ2core (renames st) ctx typ
        ctx' <- foldM (processCommand $ isEntry st) ctx cmds
        put st{typingEnv = typenv', renames = ns', context = ctx'}

processCommand :: (MonadThrow m, MonadIO m) => Bool -> Context -> Command -> Plato m Context
processCommand _ ctx Import{} = return ctx
processCommand _ ctx (Bind name bind) = return $ V.cons (name, bind) ctx
processCommand opt ctx (Eval t) = do
        when opt $ printResult ctx (unLoc t)
        return ctx

processModule :: (MonadThrow m, MonadIO m) => Located ModuleName -> Plato m ()
processModule (L sp mod) = do
        st <- get
        let imped_list = importedList st
            impng_list = importingList st
        when (mod `elem` imped_list) $ return ()
        when (mod `elem` impng_list) $ throwLocErr sp "Cyclic dependencies"
        put st{isEntry = False, importingList = mod : impng_list}
        base_path <- gets basePath
        processFile (base_path </> mod2path mod)
        put st{importedList = mod : imped_list}