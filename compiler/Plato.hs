module Plato (runPlato) where

import Control.Monad.IO.Class
import Prettyprinter.Render.Text

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp

-- import Plato.RunCore
import Plato.TypToCore
import Plato.Typing

runPlato :: FilePath -> IO ()
runPlato src = catchError $ unPlato (compile src) =<< initSession

compile :: FilePath -> Plato ()
compile src = do
        pssyn <- parseFile src
        pssyn' <- nicify pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typing typsyn
        coresyn <- typ2core typsyn'
        liftIO $ putDoc $ prettyCommands coresyn
        undefined

-- coresyn' <- runCore coresyn
-- liftIO $ mapM_ printResult coresyn'

{-repl :: [FilePath] -> IO ()
repl files = do
        env <- initSession
        _ <- unPlato (mapM compile files) env
        runInputT defaultSettings (catchError $ unPlato loop env)
    where
        loop :: (MonadIO m, MonadMask m) => PlatoT (InputT m) ()
        loop = do
                minput <- lift $ getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just inp -> continueError (return ()) $ dynCompile (T.pack inp)-}