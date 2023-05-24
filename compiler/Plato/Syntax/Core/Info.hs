module Plato.Syntax.Core.Info (
        NameInfo (Dummy),
        mkInfo,
        mkInfoFromName,
        actualName,
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name

data NameInfo
        = NameInfo Span Name
        | Dummy
        deriving (Eq, Show)

mkInfo :: Ident -> NameInfo
mkInfo id = NameInfo (getLoc id) (nameIdent id)

mkInfoFromName :: Name -> NameInfo
mkInfoFromName = NameInfo NoSpan

actualName :: NameInfo -> Name
actualName (NameInfo _ x) = x
actualName Dummy = dummyVN