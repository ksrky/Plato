module Plato.Core.Debug where

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name

data Info = Info {info_loc :: Span, actual_name :: Maybe Name} deriving (Eq, Show)

mkInfo :: Ident -> Info
mkInfo id = Info (getLoc id) (Just (nameIdent id))

mkInfoFromSpan :: Span -> Info
mkInfoFromSpan sp = Info sp Nothing

dummyInfo :: Info
dummyInfo = Info NoSpan Nothing

actualName :: Info -> Name
actualName Info{actual_name = Just x} = x
actualName Info{actual_name = Nothing} = unreachable "no name"