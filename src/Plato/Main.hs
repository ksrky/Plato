{-# LANGUAGE OverloadedStrings #-}

module Plato.Main where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Interaction.Monad
import Plato.Syntax.Core
import qualified Plato.Syntax.Parsing as P
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Console.Haskeline

runPlato :: String -> IO ()
runPlato src = catchError $ evalStateT (processFile src) (initPlatoState src)

repl :: IO ()
repl = runInputT defaultSettings (catchError $ loop $ initPlatoState "")
    where
        loop st = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just input -> do
                                st' <- liftIO $ process (T.pack input) `execStateT` st
                                loop st'

processFile :: (MonadThrow m, MonadIO m) => String -> Plato m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        -- processing Parsing
        ps <- src2ps input
        -- processing Imports
        let imps = P.importDecls ps
        mapM_ processModule imps
        -- processing Typing
        typenv <- gets typingEnv
        (typ, typenv') <- ps2typ typenv ps
        -- processing Core
        ns <- gets renames
        ctx <- gets context
        (ns', cmds) <- typ2core ns ctx typ
        ctx' <- foldM processCommand ctx cmds
        modify $ \s -> s{typingEnv = typenv `M.union` typenv', renames = ns', context = ctx'}

processCommand :: (MonadThrow m, MonadIO m) => Context -> Command -> Plato m Context
processCommand ctx Import{} = return ctx
processCommand ctx (Bind name bind) = return $ V.cons (name, bind) ctx
processCommand ctx (Eval t) = do
        printResult ctx (unLoc t)
        return ctx

processModule :: (MonadThrow m, MonadIO m) => Located ModuleName -> Plato m ()
processModule (L sp mod) = do
        imped_list <- gets importedList
        impng_list <- gets importingList
        when (mod `elem` imped_list) $ return ()
        when (mod `elem` impng_list) $ throwLocErr sp "Cyclic dependencies"
        modify $ \s -> s{importingList = mod : impng_list}
        base_path <- gets basePath
        processFile (base_path ++ mod2path mod)
        modify $ \s -> s{importingList = impng_list, importedList = mod : imped_list}