module Plato.Typing.Rename where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

type Names = [(Name, Located Name)]

data RenameState = RenameState
        { names :: Names
        , level :: Int
        }

mkProj :: Located Name -> Located Name -> Expr
mkProj r = ProjE (noLoc $ VarE r)

mkValue :: Names -> Located Name -> Located Expr
mkValue names lx@(L sp x) = case lookup x names of
        Just r -> L sp (mkProj r lx)
        Nothing -> L sp (VarE lx)

fresh :: RenameState -> Located Name --tmp
fresh memo = noLoc $ str2varName ("_" ++ show (level memo))