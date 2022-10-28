module Plato.Interaction.Packages where

import qualified Data.Text as T
import Plato.Common.Error
import Plato.Common.Name

data Package = Package String [String]

packages :: [Package]
packages =
        [ Package
                "base"
                [ "Plato.Base"
                , "Plato.Bool"
                , "Plato.Either"
                , "Plato.Error"
                , "Plato.List"
                , "Plato.Maybe"
                , "Plato.Nat"
                , "Plato.Pair"
                ]
        ]

lookupModule :: ModuleName -> Maybe String
lookupModule modn = case paths of
        [] -> Nothing
        [path] -> Just $ "libs/" ++ path
        _ -> unreachable "Conflicting module names"
    where
        paths = [p ++ "/" ++ m | Package p ms <- packages, m <- ms, m == mod2path modn]