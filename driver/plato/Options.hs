module Options where

import Data.Text qualified as T
import Info
import Options.Applicative

data Options = Options
        { searchPaths :: [FilePath]
        , logPath :: !(Maybe FilePath)
        , isDebug :: !Bool
        , printParsed :: !Bool
        , printTyped :: !Bool
        , printCore :: !Bool
        }
        deriving (Eq, Show)

data Command
        = REPL [FilePath] Options
        | Run [FilePath] Options
        | Version String
        deriving (Eq, Show)

pSearchPaths :: Parser [FilePath]
pSearchPaths =
        concat
                <$> optional
                        ( option
                                strs
                                ( short 'i'
                                        <> metavar "PATHS..."
                                        <> help "setting search paths"
                                )
                        )
    where
        strs :: ReadM [String]
        strs = map T.unpack . T.splitOn ":" . T.pack <$> str

pLogPath :: Parser (Maybe FilePath)
pLogPath =
        optional
                ( strOption
                        ( long "log"
                                <> metavar "[PATH]"
                                <> help "setting log output path"
                        )
                )

pIsDebug :: Parser Bool
pIsDebug =
        switch
                ( long "debug"
                        <> short 'd'
                        <> help "Enable debug mode"
                )

pPrintParsed :: Parser Bool
pPrintParsed = switch (long "print-parsed" <> help "Print parsed program")

pPrintTyped :: Parser Bool
pPrintTyped = switch (long "print-typed" <> help "Print typed program")

pPrintCore :: Parser Bool
pPrintCore = switch (long "print-core" <> help "Print core program")

pOptions :: Parser Options
pOptions =
        Options
                <$> pSearchPaths
                <*> pLogPath
                <*> pIsDebug
                <*> pPrintParsed
                <*> pPrintTyped
                <*> pPrintCore

pREPL :: Parser Command
pREPL = REPL <$> many (argument str (metavar "FILES...")) <*> pOptions

pRun :: Parser Command
pRun = Run <$> many (argument str (metavar "FILES...")) <*> pOptions

pVersion :: Parser Command
pVersion =
        flag'
                (Version infoVersion)
                ( long "version"
                        <> short 'v'
                        <> help "print version"
                )

pCommand :: Parser Command
pCommand =
        subparser
                ( command "run" (info pRun idm)
                )
                <|> pREPL
                <|> pVersion

runWithCommand :: IO Command
runWithCommand =
        execParser $
                info
                        (pCommand <**> helper)
                        ( fullDesc
                                <> progDesc "Compile Plato program and evaluate it on the core language."
                                <> header ("Plato version " ++ infoVersion ++ ", Copyright ksrk (c) 2022.")
                        )
