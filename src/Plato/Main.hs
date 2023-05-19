module Plato.Main (runPlato) where
 
import Control.Monad.IO.Class 

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Parsing
import Plato.PsToTyp
import Plato.RunCore
import Plato.Scoping
import Plato.TypToCore
import Plato.Typing

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
{-data PlatoEnv = PlatoEnv
        { plt_filePath :: FilePath
        , plt_directoryPath :: FilePath
        , plt_uniq :: Uniq
        , plt_scope :: Scope
        , plt_typEnv :: T.TypEnv
        , plt_coreEnv :: C.CoreEnv
        }

instance HasInfo PlatoEnv where
        getFilePath = plt_filePath
        getDirectoryPath = plt_directoryPath

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data Session = Session {unSession :: !(IORef PlatoEnv)}

class MonadReader Session m => PlatoMonad m where
        getSession :: m PlatoEnv
        setSession :: PlatoEnv -> m ()
        setUniq :: Uniq -> m ()
        setUniq uniq = do
                env <- getSession
                setSession env{plt_uniq = uniq}

----------------------------------------------------------------
-- Plato
----------------------------------------------------------------
type Plato = ReaderT Session IO

unPlato :: Plato a -> Session -> IO a
unPlato = runReaderT

instance PlatoMonad Plato where
        getSession = do
                Session ref <- ask
                liftIO $ readIORef ref
        setSession env = do
                Session ref <- ask
                liftIO $ writeIORef ref env-}

runPlato :: FilePath -> IO ()
runPlato src = catchError $ unPlato (process src) undefined

process :: FilePath -> Plato ()
process src = do
        pssyn <- parseFile src
        pssyn' <- scopingProgram pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typingProgram typsyn
        coresyn <- typ2core typsyn'
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'
