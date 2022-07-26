module Plato.Syntax.Canon where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Syntax.Abstract

import Control.Exception.Safe
import Control.Monad.State

class Canon a where
        reorganize :: MonadThrow m => a -> m a

instance Canon Expr where
        reorganize (LamExpr fi xs e) = return $ foldr (LamExpr fi . (: [])) e xs
        reorganize (LetExpr fi ds e) = return $ foldr (LetExpr fi . (: [])) e ds
        reorganize exp@(CaseExpr fi e alts) = return exp -- tmp
        reorganize e = return e

instance Canon Type where
        reorganize (AllType fi xs ty) = return $ foldr (AllType fi . (: [])) ty xs
        reorganize ty = return ty

instance Canon Decl where
        reorganize d = return d

instance Canon TopDecl where
        reorganize td = return td

instance Canon a => Canon [a] where
        reorganize xs = mapM reorganize xs
