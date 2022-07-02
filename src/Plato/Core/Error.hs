module Plato.Core.Error where

import qualified Plato.Common.Position as P

type Error a = Either Error' a

data Error' = Error' {message :: String, position :: P.Pos}

instance Show Error' where
    show (Error' msg pos) = show pos ++ " " ++ msg

err :: P.Pos -> String -> Error a
err pos msg = Left $ Error' msg pos

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s