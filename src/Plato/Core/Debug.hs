module Plato.Core.Debug where

import Plato.Common.Location
import Plato.Common.Name

data Info = Info {info_loc :: Span, actual_name :: Maybe Name} deriving (Eq, Show)

mkInfo :: Located Name -> Info
mkInfo (L sp x) = Info sp (Just x)

mkInfoFromSpan :: Span -> Info
mkInfoFromSpan sp = Info sp Nothing

dummyInfo :: Info
dummyInfo = Info NoSpan Nothing