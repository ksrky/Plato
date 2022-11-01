module Plato.Interaction.Monad where

import Plato.Core.Context

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Core.Eval
import Plato.Core.Pretty
import Plato.Syntax.Core
import Plato.Syntax.Typing
import Plato.Typing.Renamer
import Prettyprinter.Render.Text (putDoc)

data PlatoState = PlatoState
        { basePath :: String
        , context :: Context
        , typingEnv :: TypEnv
        , renames :: Names
        , importedList :: [ModuleName]
        , importingList :: [ModuleName]
        }

initPlatoState :: String -> PlatoState
initPlatoState src =
        PlatoState
                { basePath = reverse $ getBasePath $ reverse src
                , context = emptyContext
                , typingEnv = M.empty
                , renames = []
                , importedList = []
                , importingList = []
                }
    where
        getBasePath :: String -> String
        getBasePath [] = ""
        getBasePath ('/' : xs) = '/' : xs
        getBasePath (_ : xs) = getBasePath xs

type Plato m a = StateT PlatoState m a

printResult :: MonadIO m => Context -> Term -> Plato m ()
printResult ctx t = liftIO $ putDoc $ ppr ctx $ eval ctx t
