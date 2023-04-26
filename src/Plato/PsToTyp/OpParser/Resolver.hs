{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Plato.PsToTyp.OpParser.Resolver where

import Control.Exception.Safe
import Control.Monad

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Location
import Plato.Common.Path

-- Fixity resolution
data Tok a = TTerm (Located a) | TOp Path Fixity deriving (Eq, Show)

instance GetLoc (Tok a) where
        getLoc (TTerm tm) = getLoc tm
        getLoc (TOp op _) = getLoc op

parse :: forall a m. MonadThrow m => (Located a -> Path -> Located a -> a) -> [Tok a] -> m (Located a)
parse infixtm toks = do
        (result, rest) <- parseTerm (Fixity (-1) Nonfix) toks
        unless (null rest) $ unreachable "Token stack remained"
        return result
    where
        parseTerm :: Fixity -> [Tok a] -> m (Located a, [Tok a])
        parseTerm fix (TTerm lhs : rest) = parseOp fix lhs rest
        parseTerm _ _ = unreachable "malformed infix expression"

        parseOp :: Fixity -> Located a -> [Tok a] -> m (Located a, [Tok a])
        parseOp lfix@(Fixity lprec ldir) lhs (tokop@(TOp op fix@(Fixity prec dir)) : rest)
                | lprec == prec && (ldir /= dir || ldir == Nonfix) =
                        throwPsErr
                                (combineSpans (getLoc lhs) (getLoc tokop))
                                "Error at parsing infix expression"
                | lprec > prec || (lprec == prec && ldir == Leftfix) = return (lhs, tokop : rest)
                | otherwise = do
                        (rhs, rest') <- parseTerm fix rest
                        parseOp lfix (sL lhs rhs (infixtm lhs op rhs)) rest'
        parseOp _ _ _ = unreachable "malformed infix expression"