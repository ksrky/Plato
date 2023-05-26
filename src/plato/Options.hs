module Options where

import Options.Applicative

type Package = String

data Options
        = REPL [FilePath]
        | Run FilePath
        | Version
        deriving (Eq, Show)

repl :: Parser Options
repl = REPL <$> many (argument str (metavar "FILES..."))

run :: Parser Options
run = Run <$> argument str (metavar "FILE...")

version :: Parser Options
version =
        flag'
                Version
                ( long "version"
                        <> short 'v'
                        <> help "print version"
                )

opts :: Parser Options
opts =
        subparser
                ( command "run" (info run idm)
                )
                <|> repl
                <|> version

runWithOptions :: IO Options
runWithOptions =
        execParser $
                info
                        (opts <**> helper)
                        ( fullDesc
                                <> progDesc "Print a greeting for TARGET"
                                <> header "hello - a test for optparse-applicative"
                        )