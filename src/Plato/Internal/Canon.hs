module Plato.Internal.Canon where

import Plato.Common.Info
import Plato.Internal.Syntax

import Plato.Common.Error
import Plato.Common.Name

import Control.Exception.Safe
import Data.List

class Canon a where
        canon :: a -> a

{-}
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
        canon (CaseExpr' fi [e] alts) = undefined
            where
                reorder :: [([Pat], Expr)] -> [([Pat], Expr)]
                reorder [] = []
                reorder (alt : alts) =
                        let ([ConPat fi1 l ps], _) = alt
                            (matches, notmatches) = (`partition` (alt : alts)) $ \(pi, _) -> case pi of
                                [ConPat _ li _] | l == li -> True
                                [_] -> False
                                _ -> unreachable "One pattern required"
                            matches' = (`map` matches) $ \([ConPat fi1 li1 pi1], ei) -> (pi1, ei)
                            patnames = map (str2varName . show) [1 .. length ps]
                            patexprs = map (VarExpr dummyInfo) patnames
                            pats = map (AnyPat dummyInfo . Just) patnames
                            body = CaseExpr' fi1 patexprs matches'
                         in ([ConPat fi1 l pats], body) : reorder notmatches
        canon CaseExpr'{} = unreachable "One expr required"
        canon e = e
-}

instance Canon Pat where
        canon (ConPat fi li pi) = undefined
        canon (AnyPat fi mli) = undefined