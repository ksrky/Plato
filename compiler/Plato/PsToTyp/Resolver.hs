module Plato.PsToTyp.Resolver (Tok (..), parseTok) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Ident
import Plato.Common.Location

data Tok a
    = TTerm a
    | TOp Ident
    deriving (Eq)

instance (Show a) => Show (Tok a) where
    show (TTerm tm)                     = show tm
    show (TOp Ident{fixityIdent = fix}) = show fix

instance (HasLoc a) => HasLoc (Tok a) where
    getLoc (TTerm tm) = getLoc tm
    getLoc (TOp op)   = getLoc op

parseTok ::
    forall a m.
    (HasCallStack, HasLoc a, MonadIO m, MonadThrow m) =>
    (Ident -> a -> a -> a) -> [Tok a] -> m a
parseTok elab toks = do
    (result, rest) <- parseTerm (Fixity (pred minBound) Nonfix) toks
    unless (null rest) $ unreachable "Token stack remained"
    return result
  where
    parseTerm :: Fixity -> [Tok a] -> m (a, [Tok a])
    parseTerm fix (TTerm lhs : rest) = parseOp fix lhs rest
    parseTerm _ _                    = unreachable "malformed infix expression"
    parseOp :: Fixity -> a -> [Tok a] -> m (a, [Tok a])
    parseOp _ result [] = return (result, [])
    parseOp lfix@(Fixity lprec ldir) lhs (tokop@(TOp op) : rest)
        | lprec == prec && (ldir /= dir || ldir == Nonfix) =
                throwLocErr (getLoc lhs <> getLoc op) "Error at parsing infix expression"
        | lprec > prec || (lprec == prec && ldir == Leftfix) = return (lhs, tokop : rest)
        | otherwise = do
                (rhs, rest') <- parseTerm (fixityIdent op) rest
                parseOp lfix (elab op lhs rhs) rest'
        where
        Fixity prec dir = fixityIdent op
    parseOp _ _ _ = unreachable "malformed infix expression"
