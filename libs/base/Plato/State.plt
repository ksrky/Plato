module Plato.State

import Plato.Base
import Plato.Pair

data State s a = State (s -> Pair a s)

get :: {s} State s s
get = State $ \s -> Pair s s

put :: {s} s -> State s Unit
put s = State $ \_ -> (Unit, s)

modify :: {s} (s -> s) -> State s Unit
modify f = State $ \s -> Pair Unit (f s)

gets :: {s a} (s -> a) -> State s a
gets f = State $ \s -> Pair (f s) s

runState :: {s a} State s a -> s -> Pair a s
runState st s = case st of
        State f -> f s

evalState :: {s a} State s a -> s -> a
evalState st = fst . runState

execState :: {s a} State s a -> s -> s
execState = snd . runState

