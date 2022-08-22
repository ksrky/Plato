module Plato.Abstract.Fixity where

import qualified Plato.Abstract.Syntax as A
import Plato.Common.Info
import Plato.Common.Name

type Prec = Int

data Op = Op Name Prec Fixity
        deriving (Eq, Show)

data Fixity = Leftfix | Rightfix | Nonfix
        deriving (Eq, Show)

data Exp = Exp A.Expr | OpApp Info Exp Op Exp
        deriving (Eq, Show)

data Tok = TExp Exp | TOp Info Op
        deriving (Eq, Show)

resolve :: [Tok] -> Maybe Exp
resolve tokens = fst <$> parseNeg (Op dummyVarName (-1) Nonfix) tokens
    where
        parseNeg :: Op -> [Tok] -> Maybe (Exp, [Tok])
        parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
        parseNeg _ _ = undefined

        parse :: Op -> Exp -> [Tok] -> Maybe (Exp, [Tok])
        parse _ e1 [] = Just (e1, [])
        parse op1@(Op _ prec1 fix1) e1 (TOp fi op2@(Op _ prec2 fix2) : rest)
                | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = Nothing
                | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = Just (e1, TOp fi op2 : rest)
                | otherwise = do
                        (r, rest') <- parseNeg op2 rest
                        parse op1 (OpApp fi e1 op2 r) rest'
        parse _ _ _ = undefined

exp2expr :: Exp -> A.Expr
exp2expr (Exp expr) = expr
exp2expr (OpApp fi e1 (Op op prec fix) e2) = A.OpExpr fi (exp2expr e1) op (exp2expr e2)
