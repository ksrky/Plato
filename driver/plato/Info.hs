{-# LANGUAGE TemplateHaskell #-} 

module Info where

import Info.TH

compilerInfo :: CompilerInfo
compilerInfo = $(loadJSONFile @CompilerInfo infoPath)

infoVersion :: String
infoVersion = info_version compilerInfo