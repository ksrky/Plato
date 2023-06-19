module Plato.Driver.Import where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader 
import Data.Set qualified as S
import Data.Text qualified as T
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
        (MonadReader env m, HasImporting env, MonadThrow m) =>
        Located T.Text ->
        m ()
checkCyclicImport (L sp filename) = do
        impngSet <- asks getImporting
        when (filename `S.member` impngSet) $ throwLocErr sp "Cyclic dependencies"

unlessImported ::
        (MonadReader env m, HasInfo env, HasImported env, MonadIO m, Monoid a) =>
        Located T.Text ->
        (FilePath -> m a) ->
        ReaderT Importing m a
unlessImported (L _ filename) parse = do
        impedSet <- getImported =<< lift ask
        if filename `S.member` impedSet
                then return mempty
                else do
                        dirpath <- getDirectoryPath =<< lift ask
                        res <- local (extendImporting filename) $ lift $ parse (joinPath [dirpath, T.unpack filename ++ ".plt"])
                        lift $ extendImported filename =<< ask
                        return res