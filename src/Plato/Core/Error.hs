module Plato.Core.Error where

import Plato.Common.Info (Pos)

type Error a = Either Error' a

data Error' = Error' {message :: String, position :: Pos}

instance Show Error' where
    show (Error' msg pos) = show pos ++ " " ++ msg

err :: Pos -> String -> Error a
err pos msg = Left $ Error' msg pos

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s