module Plato.Driver.Import where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S
import Data.Text qualified as T
import System.FilePath

import Plato.Common.Error
import Plato.Common.Location
import Plato.Driver.Info

type Imported = S.Set T.Text
type Importing = S.Set T.Text

class HasImported a where
        getImported :: MonadIO m => a -> m Imported
        setImported :: MonadIO m => Imported -> a -> m ()
        extendImported :: MonadIO m => T.Text -> a -> m ()
        extendImported filename env = do
                impedSet <- getImported env
                setImported (S.insert filename impedSet) env

class HasImporting a where
        getImporting :: MonadIO m => a -> m Imported
        setImporting :: MonadIO m => Imported -> a -> m ()

checkCyclicImport ::
        (MonadReader env m, HasImporting env, MonadIO m, MonadThrow m) =>
        Located T.Text ->
        m ()
checkCyclicImport (L sp filename) = do
        impngSet <- getImporting =<< ask
        when (filename `S.member` impngSet) $ throwLocErr sp "Cyclic dependencies"

unlessImported ::
        (MonadReader env m, HasInfo env, HasImported env, MonadIO m, Monoid a) =>
        Located T.Text ->
        (FilePath -> m a) ->
        m a
unlessImported (L _ filename) parse = do
        impedSet <- getImported =<< ask
        if filename `S.member` impedSet
                then return mempty
                else do
                        dirpath <- getDirectoryPath =<< ask
                        res <- parse (joinPath [dirpath, T.unpack filename ++ ".plt"])
                        extendImported filename =<< ask
                        return res