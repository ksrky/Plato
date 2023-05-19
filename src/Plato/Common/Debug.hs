module Plato.Common.Debug where

class HasInfo a where
        getFilePath :: a -> FilePath
        getDirectoryPath :: a -> FilePath