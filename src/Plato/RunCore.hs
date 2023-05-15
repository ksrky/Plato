module Plato.RunCore (runCore, printResult) where

import Control.Monad.State

import Plato.Core.Env
import Plato.Core.Eval
import Plato.Syntax.Core

runCore :: (MonadState env m, HasCoreEnv env) => [Command] -> m [Term]
runCore cmds = do
        env <- gets getEnv
        let (res, env') = evalCommands env cmds
        modify $ modifyEnv $ const env'
        return res

evalCommands :: CoreEnv -> [Command] -> ([Term], CoreEnv)
evalCommands env [] = ([], env)
evalCommands env (Bind x b : cmds) = evalCommands (addBinding x b env) cmds
evalCommands env (Eval t : cmds) =
        let (res, env') = evalCommands env cmds
         in (eval env t : res, env')

printResult :: Term -> IO ()
printResult = undefined