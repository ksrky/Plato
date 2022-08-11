module Plato.Internal.Canon where

import Plato.Common.Info
import Plato.Internal.Syntax

import Control.Exception.Safe
import Data.List
import Plato.Common.Name

class Canon a where
        canon :: a -> a

instance Canon Expr where
        canon (CaseExpr fi e alts) = undefined
            where
                def = last alts
                linearize :: [(Pat, Expr)] -> Expr
                linearize [] = CaseExpr fi e [def]
                linearize [alt] = CaseExpr fi e (alt : [def])
                linearize (alt : alts) =
                        let (ConPat _ l _, _) = alt
                            (matches, notmatches) = (`partition` alts) $ \(pi, _) -> case pi of
                                ConPat _ li _ | l == li -> True
                                _ -> False
                         in CaseExpr fi e (alt : matches ++ [(AnyPat dummyInfo Nothing, linearize notmatches)])
        canon e = e
