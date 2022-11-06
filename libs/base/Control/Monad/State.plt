module Control.State

import Plato.Base
import Plato.Pair

data MonadState s = MonadState ({m} Monad m -> m s) ({m} Monad m -> s -> m Unit)

get : {s} MonadState s -> {m} Monad m -> m s
get st = case st of
        MonadState x _ -> x

put : {s} MonadState s -> {m} Monad m -> s -> m Unit
put st = case st of
        MonadState _ y -> y

state : {s} MonadState s -> {m} Monad m -> (s -> (a, s)) -> m a
state st mn f =
        let Pair a s = f (get st mn)
         in bind mn (put st mn s) (\_ -> return mn a)

modify :: {s} (s -> s) -> State s Unit
modify f = State $ \s -> Pair Unit (f s)

gets :: {s a} (s -> a) -> State s a
gets f = State $ \s -> Pair (f s) s

runState :: {s a} State s a -> s -> Pair a s
runState st s = case st of State f -> f s

evalState :: {s a} State s a -> s -> a
evalState st = fst . runState

execState :: {s a} State s a -> s -> s
execState = snd . runState