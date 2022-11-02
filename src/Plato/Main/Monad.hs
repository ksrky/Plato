module Plato.Main.Monad where

import Plato.Core.Context

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Core.Eval
import Plato.Core.Pretty as C
import Plato.Main.Pretty as I
import Plato.Syntax.Core
import Plato.Syntax.Typing
import Plato.Typing.Renamer
import Prettyprinter.Render.Text (putDoc)

data PlatoState = PlatoState
        { isEntry :: Bool
        , basePath :: FilePath
        , context :: Context
        , typingEnv :: TypEnv
        , renames :: Names
        , importedList :: [ModuleName]
        , importingList :: [ModuleName]
        }

initPlatoState :: PlatoState
initPlatoState =
        PlatoState
                { isEntry = True
                , basePath = "."
                , context = emptyContext
                , typingEnv = M.empty
                , renames = []
                , importedList = []
                , importingList = []
                }

type Plato m a = StateT PlatoState m a

printResult :: MonadIO m => Context -> Term -> Plato m ()
printResult ctx t = liftIO $ putDoc $ I.ppr ctx $ eval ctx t

debugResult :: MonadIO m => Context -> Term -> Plato m ()
debugResult ctx t = liftIO $ do
        putDoc $ C.ppr ctx $ eval ctx t
        putStrLn ""
        print $ eval ctx t