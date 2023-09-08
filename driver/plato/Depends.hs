{-# LANGUAGE LambdaCase #-}

module Depends where

import Control.Exception.Safe
import System.Directory
import System.FilePath

readDepend :: FilePath -> IO [FilePath]
readDepend path =
        try (readFile (path </> ".depend")) >>= \case
                Right content -> return $ map (path </>) (lines content)
                Left (_ :: SomeException) -> filterPaths <$> listDirectory path

filterPaths :: [FilePath] -> [FilePath]
filterPaths = filter (\f -> takeExtension f == ".pla")