module Plato.Driver.Import where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Set qualified as S
import Data.Text qualified as T
import Prettyprinter
import System.Directory
import System.FilePath

import Plato.Common.Error
import Plato.Common.Location
import Plato.Driver.Info

type Imported = S.Set T.Text
type Importing = S.Set T.Text

emptyImporting :: Importing
emptyImporting = S.empty

class HasImported a where
        getImported :: MonadIO m => a -> m Imported
        setImported :: MonadIO m => Imported -> a -> m ()
        extendImported :: MonadIO m => T.Text -> a -> m ()
        extendImported filename env = do
                impedSet <- getImported env
                setImported (S.insert filename impedSet) env

class HasImporting a where
        getImporting :: a -> Imported
        modifyImporting :: (Imported -> Imported) -> a -> a
        extendImporting :: T.Text -> a -> a
        extendImporting filename = modifyImporting (S.insert filename)

instance HasImporting Importing where
        getImporting = id
        modifyImporting = id

checkCyclicImport ::
        (MonadReader e m, HasImporting e, MonadThrow m) =>
        Located T.Text ->
        m ()
checkCyclicImport (L sp filename) = do
        impngSet <- asks getImporting
        when (filename `S.member` impngSet) $ throwLocErr sp "Cyclic dependencies"

unlessImported ::
        (MonadReader e m, HasInfo e, HasImported e, MonadIO m, MonadThrow m, Monoid a) =>
        Located T.Text ->
        (FilePath -> m a) ->
        ReaderT Importing m a
unlessImported (L _ filename) parse = do
        impedSet <- getImported =<< lift ask
        if filename `S.member` impedSet
                then return mempty
                else do
                        filepath <- lift $ getImportPath filename
                        res <- local (extendImporting filename) (lift $ parse filepath)
                        lift $ extendImported filename =<< ask
                        return res

getImportPath :: (MonadReader e m, HasInfo e, MonadIO m, MonadThrow m) => T.Text -> m FilePath
getImportPath filename = do
        curpath <- getCurrentDirPath =<< ask
        libpaths <- getLibraryPaths =<< ask
        filepaths <- execWriterT $ forM (curpath : libpaths) $ \dirpath -> do
                let filepath = joinPath [dirpath, T.unpack filename ++ platoExt]
                flag <- liftIO $ doesFileExist filepath
                when flag $ tell [filepath]
        case filepaths of
                [] -> throwError $ hsep ["Couldn't import", pretty filename]
                p : _ -> return p