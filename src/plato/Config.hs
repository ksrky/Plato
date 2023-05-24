module Config where

data Config = Config
        { cfg_version :: String
        }

config :: Config
config =
        Config
                { cfg_version = "1.0.0"
                }