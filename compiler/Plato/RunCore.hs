module Plato.RunCore (runCore, printResult) where

import Plato.Core.Env
import Plato.Core.Eval
import Plato.Driver.Monad
import Plato.Syntax.Core

runCore :: PlatoMonad m => [Command] -> m [Term]
runCore cmds = return $ fst $ evalCommands initCoreEnv cmds

evalCommands :: CoreEnv -> [Command] -> ([Term], CoreEnv)
evalCommands env [] = ([], env)
evalCommands env (Bind fi bind : cmds) = evalCommands (addBinding (actualName fi) bind env) cmds
evalCommands env (Eval t : cmds) =
        let (res, env') = evalCommands env cmds
         in (eval env t : res, env')

printResult :: Term -> IO ()
printResult = undefined