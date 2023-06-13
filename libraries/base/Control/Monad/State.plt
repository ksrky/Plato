import Plato.Base
import Plato.Pair

data MonadState s where
    MonadState : ({m} Monad m -> m s) -> ({m} Monad m -> s -> m Unit) -> MonadState s

get : {s} MonadState s -> {m} Monad m -> m s
get (MonadState x _) x

put : {s} MonadState s -> {m} Monad m -> s -> m Unit
put (MonadState _ y) = y

state : {s} MonadState s -> {m} Monad m -> (s -> a :,: s) -> m a
state st mn f =
        let a :,: s = f (get st mn)
         in bind mn (put st mn s) (\_ -> return mn a)

modify :: {s} (s -> s) -> State s Unit
modify f = State $ \s -> Unit :,: (f s)

gets :: {s a} (s -> a) -> State s a
gets f = State $ \s -> f s :,: s

runState :: {s a} State s a -> s -> a :,: s
runState (State f) s = f s

evalState :: {s a} State s a -> s -> a
evalState st = fst . runState

execState :: {s a} State s a -> s -> s
execState = snd . runState