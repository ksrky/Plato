{-# LANGUAGE LambdaCase #-}

module Plato.Parsing.OpParser (FixityEnv, HasFixityEnv (..), OpParser (..), opParseTop) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import Data.Map.Strict qualified as M
import Data.Maybe

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Parsing.OpParser.Resolver
import Plato.Parsing.Token
import Plato.Syntax.Parsing

type FixityEnv = M.Map Name Fixity

class HasFixityEnv a where
        getFixityEnv :: a -> FixityEnv
        modifyFixityEnv :: (FixityEnv -> FixityEnv) -> a -> a
        setFixityEnv :: FixityEnv -> a -> a
        setFixityEnv = modifyFixityEnv . const

instance HasFixityEnv FixityEnv where
        getFixityEnv = id
        modifyFixityEnv = id

class Linearize a where
        linearize :: (MonadReader e m, HasFixityEnv e, MonadThrow m) => Located a -> m [Tok a]

instance Linearize Expr where
        linearize (L _ (BinE lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- asks getFixityEnv
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize exp = do
                exp' <- opParse exp
                return [TTerm exp']

instance Linearize Pat where
        linearize (L _ (BinP lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- asks getFixityEnv
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize pat = do
                pat' <- opParse pat
                return [TTerm pat']

instance Linearize Type where
        linearize (L _ (BinT lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                env <- asks getFixityEnv
                -- TODO: infix declaration does not distinguish ConOp or TyConOp
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op){nameSpace = ConName} env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize ty = do
                ty' <- opParse ty
                return [TTerm ty']

-- | Operator parser
class OpParser a where
        opParse :: (MonadReader e m, HasFixityEnv e, MonadThrow m) => a -> m a

instance OpParser [Token] where
        opParse = return

instance OpParser LExpr where
        opParse (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE fun arg -> AppE <$> opParse fun <*> opParse arg
                        BinE{} -> do
                                toks <- linearize (L sp exp)
                                unLoc <$> parse BinE toks
                        LamE pats exp -> LamE <$> mapM opParse pats <*> opParse exp
                        LetE decs body -> do
                                (decs', body') <- opParseLoc (decs, body)
                                return $ LetE decs' body'
                        CaseE match alts -> do
                                match' <- opParse match
                                alts' <- mapM (\(p, e) -> (,) <$> opParse p <*> opParse e) alts
                                return $ CaseE match' alts'
                        AnnE exp ann_ty -> AnnE <$> opParse exp <*> opParse ann_ty
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
                        BinP{} -> do
                                toks <- linearize (L sp pat)
                                unLoc <$> parse BinP toks
                        AnnP pat ann_ty -> AnnP <$> opParse pat <*> opParse ann_ty
                        FactorP p -> unLoc <$> opParse p

instance OpParser LType where
        opParse (L sp ty) =
                L sp <$> case ty of
                        VarT{} -> return ty
                        ConT{} -> return ty
                        ArrT arg res -> ArrT <$> opParse arg <*> opParse res
                        AllT tvs body -> AllT tvs <$> opParse body
                        AppT fun arg -> AppT <$> opParse fun <*> opParse arg
                        BinT{} -> do
                                toks <- linearize (L sp ty)
                                unLoc <$> parse BinT toks
                        FactorT ty -> unLoc <$> opParse ty

opParseLoc :: (MonadReader e m, HasFixityEnv e, MonadThrow m, OpParser a) => ([LLocDecl], a) -> m ([LLocDecl], a)
opParseLoc (decs, a) = local (modifyFixityEnv $ \env -> foldr (uncurry M.insert) env fixmap) $ do
        rest' <- forM rest $ \case
                L sp (FunSpecD id ty) -> L sp <$> (FunSpecD id <$> opParse ty)
                L sp (FunBindD id clauses) -> L sp <$> (FunBindD id <$> mapM opParse clauses)
                dec -> return dec
        a' <- opParse a
        return (rest', a')
    where
        (fixmap, rest) = execWriter $ forM decs $ \case
                L _ (FixityD id fix) -> tell ([(nameIdent id, fix)], [])
                d -> tell ([], [d])

opParseTop :: (MonadReader e m, HasFixityEnv e, MonadThrow m) => [LTopDecl] -> m ([LTopDecl], e)
opParseTop decs = local (modifyFixityEnv $ \env -> foldr (uncurry M.insert) env fixmap) $ do
        rest' <- forM rest $ \case
                L sp (DataD id params constrs) ->
                        L sp <$> (DataD id params <$> mapM (\(con, ty) -> (con,) <$> opParse ty) constrs)
                L sp (LocalD (FunSpecD id ty)) -> L sp . LocalD <$> (FunSpecD id <$> opParse ty)
                L sp (LocalD (FunBindD id clauses)) -> L sp . LocalD <$> (FunBindD id <$> mapM opParse clauses)
                dec -> return dec
        env <- ask
        return (rest', env)
    where
        (fixmap, rest) = execWriter $ forM decs $ \case
                L _ (LocalD (FixityD id fix)) -> tell ([(nameIdent id, fix)], [])
                d -> tell ([], [d])

instance OpParser [LTopDecl] where
        opParse = (fst <$>) . opParseTop