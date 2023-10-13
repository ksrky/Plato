module Plato.Parsing.OpParser.Resolver (Tok (..), parse) where

import Control.Exception.Safe
import Control.Monad
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Ident
import Plato.Common.Location

data Tok a = TTerm a | TOp Ident Fixity deriving (Eq, Show)

instance (HasLoc a) => HasLoc (Tok a) where
        getLoc (TTerm tm) = getLoc tm
        getLoc (TOp op _) = getLoc op

parse ::
        forall a m.
        (HasCallStack, HasLoc a, MonadThrow m) =>
        (Ident -> a -> a -> a) ->
        [Tok a] ->
        m a
parse elab toks = do
        (result, rest) <- parseTerm (Fixity (pred minBound) Nonfix) toks
        unless (null rest) $ unreachable "Token stack remained"
        return result
    where
        parseTerm :: Fixity -> [Tok a] -> m (a, [Tok a])
        parseTerm fix (TTerm lhs : rest) = parseOp fix lhs rest
        parseTerm _ _ = unreachable "malformed infix expression"

        parseOp :: Fixity -> a -> [Tok a] -> m (a, [Tok a])
        parseOp _ result [] = return (result, [])
        parseOp lfix@(Fixity lprec ldir) lhs (tokop@(TOp op fix@(Fixity prec dir)) : rest)
                | lprec == prec && (ldir /= dir || ldir == Nonfix) =
                        throwLocErr
                                (getLoc lhs <> getLoc tokop)
                                "Error at parsing infix expression"
                | lprec > prec || (lprec == prec && ldir == Leftfix) = return (lhs, tokop : rest)
                | otherwise = do
                        (rhs, rest') <- parseTerm fix rest
                        parseOp lfix (elab op lhs rhs) rest'
        parseOp _ _ _ = unreachable "malformed infix expression"