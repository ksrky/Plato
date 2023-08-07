{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Info.TH where

import Data.Aeson (
        FromJSON (parseJSON),
        Result (Error, Success),
        Value,
        eitherDecode,
        fromJSON,
        withObject,
        (.:),
 )
import Data.ByteString.Lazy.Char8 qualified as C8
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))
import Language.Haskell.TH.Syntax (Lift)

buildJSONExp :: Value -> Q Exp
buildJSONExp value = [e|value|]

json :: QuasiQuoter
json = QuasiQuoter{quoteExp = buildJSONExp . parseExp}

parseExp :: String -> Value
parseExp str =
        let result = eitherDecode (C8.pack str)
         in case result of
                Left err -> error err
                Right json -> json

infoPath :: String
infoPath = "compiler/metainfo.json"

loadJSONFile :: forall a. (FromJSON a, Lift a) => FilePath -> Q Exp
loadJSONFile filename = do
        inp <- runIO $ readFile filename
        let json = parseExp inp
        case fromJSON @a json of
                Success x -> [e|x|]
                Error err -> error err

data CompilerInfo = CompilerInfo
        { info_version :: String
        }
        deriving (Show, Lift)

instance FromJSON CompilerInfo where
        parseJSON = withObject "CompilerInfo" $ \o ->
                CompilerInfo <$> o .: "version"