{-# LANGUAGE LambdaCase #-}

module Options where

import Config
import Options.Applicative

data Options
        = REPL [FilePath]
        | Run FilePath [FilePath] (Maybe FilePath)
        | Version String
        deriving (Eq, Show)

repl :: Parser Options
repl = REPL <$> many (argument str (metavar "FILES..."))

run :: Parser Options
run = Run <$> argument str (metavar "FILE...") <*> libraryPaths <*> logPath

version :: Parser Options
version =
        flag'
                (Version (cfg_version config))
                ( long "version"
                        <> short 'v'
                        <> help "print version"
                )

libraryPaths :: Parser [FilePath]
libraryPaths =
        -- tmp
        (\case Just x -> [x]; Nothing -> [])
                <$> optional
                        ( strOption
                                ( long "libs"
                                        <> short 'l'
                                        <> metavar "PATHS..."
                                        <> help "setting library paths"
                                )
                        )

logPath :: Parser (Maybe FilePath)
logPath =
        optional
                ( strOption
                        ( long "libs"
                                <> short 'l'
                                <> metavar "PATHS..."
                                <> help "setting library paths"
                        )
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
                                <> progDesc "Compile Plato program and evaluate it on the core language."
                                <> header ("Plato version " ++ cfg_version config ++ ", Copyright ksrk (c) 2022.")
                        )
