{-# LANGUAGE LambdaCase #-}

module Plato.Abstract.Canon where

import Plato.Abstract.Syntax
import Plato.Common.Error
import Plato.Common.Info

import Control.Exception.Safe
import Control.Monad.State
import Data.List

class Canon a where
        reorganize :: MonadThrow m => a -> m a

instance Canon Expr where
        reorganize (LamExpr fi xs e) = return $ foldr (LamExpr fi . (: [])) e xs
        reorganize (LetExpr fi ds e) = do
                let mi = (`findIndex` ds) $ \case
                        FuncTyDecl{} -> True
                        _ -> False
                case mi of
                        Just i -> do
                                let ds' = ds !! i : (take i ds ++ drop (i + 1) ds)
                                return $ foldr (LetExpr fi . (: [])) e ds'
                        Nothing -> throwError fi "Type annotation required"
        reorganize exp@(CaseExpr fi e alts) = return exp -- tmp
        reorganize e = return e

instance Canon Type where
        reorganize (AllType fi xs ty) = return $ foldr (AllType fi . (: [])) ty xs
        reorganize ty = return ty

instance Canon Decl where
        reorganize (FuncDecl fi f e) = FuncDecl fi f <$> reorganize e
        reorganize (FuncTyDecl fi f ty) = FuncTyDecl fi f <$> reorganize ty

instance Canon TopDecl where
        reorganize (Decl d) = Decl <$> reorganize d
        reorganize td = return td

instance Canon a => Canon [a] where
        reorganize xs = mapM reorganize xs
