{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Nicifier.OpParser (
        FixityEnv,
        initFixityEnv,
        HasFixityEnv (..),
        opParse,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import Data.Map.Strict qualified as M
import Data.Maybe

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Nicifier.OpParser.Resolver
import Plato.Syntax.Parsing

type FixityEnv = M.Map Name Fixity

initFixityEnv :: FixityEnv
initFixityEnv = M.empty

class HasFixityEnv a where
        getFixityEnv :: a -> FixityEnv
        modifyFixityEnv :: (FixityEnv -> FixityEnv) -> a -> a

instance HasFixityEnv FixityEnv where
        getFixityEnv = id
        modifyFixityEnv = id

class Linearize a where
        linearize :: MonadThrow m => Located a -> ReaderT FixityEnv m [Tok a]

instance Linearize Expr where
        linearize (L _ (InfixE lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- ask
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize exp = do
                exp' <- opParse exp
                return [TTerm exp']

instance Linearize Pat where
        linearize (L _ (InfixP lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- ask
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize pat = do
                pat' <- opParse pat
                return [TTerm pat']

instance Linearize Type where
        linearize (L _ (InfixT lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- ask
                -- TODO: infix declaration does not distinguish ConOp or TyConOp
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op){nameSpace = ConName} env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize ty = do
                ty' <- opParse ty
                return [TTerm ty']

-- | Operator parser
class OpParser a where
        opParse :: MonadThrow m => a -> ReaderT FixityEnv m a

instance OpParser LExpr where
        opParse (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE fun arg -> AppE <$> opParse fun <*> opParse arg
                        InfixE{} -> do
                                toks <- linearize (L sp exp)
                                unLoc <$> parse InfixE toks
                        LamE pats exp -> LamE <$> mapM opParse pats <*> opParse exp
                        LetE decs body -> do
                                decs' <- opParse decs
                                body' <- opParse body
                                return $ LetE decs' body'
                        CaseE match alts -> do
                                match' <- opParse match
                                alts' <- mapM (\(p, e) -> (,) <$> opParse p <*> opParse e) alts
                                return $ CaseE match' alts'
                        FactorE e -> unLoc <$> opParse e

instance OpParser Clause where
        opParse (pats, exp) = (,) <$> mapM opParse pats <*> opParse exp

instance OpParser LPat where
        opParse (L sp pat) =
                L sp <$> case pat of
                        ConP con pats -> do
                                pats' <- mapM opParse pats
                                return $ ConP con pats'
                        VarP{} -> return pat
                        WildP -> return WildP
                        InfixP{} -> do
                                toks <- linearize (L sp pat)
                                unLoc <$> parse InfixP toks
                        FactorP p -> unLoc <$> opParse p

instance OpParser LType where
        opParse (L sp ty) =
                L sp <$> case ty of
                        VarT{} -> return ty
                        ConT{} -> return ty
                        ArrT arg res -> ArrT <$> opParse arg <*> opParse res
                        AllT tvs body -> AllT tvs <$> opParse body
                        AppT fun arg -> AppT <$> opParse fun <*> opParse arg
                        InfixT{} -> do
                                toks <- linearize (L sp ty)
                                unLoc <$> parse InfixT toks
                        FactorT ty -> unLoc <$> opParse ty

instance OpParser [LDecl] where
        opParse decs = local (\env -> foldr (uncurry M.insert) env fixmap) $ forM rest $ \case
                L sp (DataD id params constrs) ->
                        L sp <$> (DataD id params <$> mapM (\(con, ty) -> (con,) <$> opParse ty) constrs)
                L sp (FunSpecD id ty) -> L sp <$> (FunSpecD id <$> opParse ty)
                L sp (FunBindD id clauses) -> L sp <$> (FunBindD id <$> mapM opParse clauses)
                dec -> return dec
            where
                (fixmap, rest) = execWriter $ forM decs $ \case
                        L _ (FixityD id fix) -> tell ([(nameIdent id, fix)], [])
                        d -> tell ([], [d])