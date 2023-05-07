{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Plato.PsToTyp.OpParser where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M

import Plato.Common.Fixity
import Plato.Common.Location
import Plato.PsToTyp.OpParser.Resolver
import Plato.Syntax.Parsing

-- Operator parser
class OpParser a where
        opParser ::
                (MonadReader env m, HasFixEnv env, MonadThrow m) => Located a -> m (Located a)
        linearize :: (MonadReader env m, HasFixEnv env, MonadThrow m) => Located a -> m [Tok a]

instance OpParser Expr where
        opParser (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE e1 e2 -> AppE <$> opParser e1 <*> opParser e2
                        OpE{} -> do
                                toks <- linearize (L sp exp)
                                unLoc <$> parse OpE toks
                        LamE alts -> do
                                LamE
                                        <$> mapM
                                                ( \(pats, body) -> (pats,) <$> opParser body
                                                )
                                                alts
                        LetE decs body -> do
                                (decs', env) <- runStateT (mapM opParserDecl decs) =<< ask
                                body' <- local (const env) $ opParser body
                                return $ LetE decs' body'
                        CaseE e alts ->
                                CaseE <$> opParser e
                                        <*> mapM
                                                ( \(pi, ei) -> (pi,) <$> opParser ei
                                                )
                                                alts
                        FactorE e -> unLoc <$> opParser e
        linearize (L _ (OpE lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                fix <- asks getFixEnv >>= return . access op
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize exp = do
                exp' <- opParser exp
                return [TTerm exp']

-- TODO: Infix patterns
-- instance OpParser Pat where

-- TODO: Infix type constructors
-- instance OpParser Type where

{-opParserDecls :: (MonadReader env m, HasFixEnv env, MonadThrow m) => [LDecl] -> m [LDecl]
opParserDecls [] = return []
opParserDecls (dec@(L sp d) : decs) = case d of
        OpenD path -> do
                newenv <- access path <$> asks getFixEnv
                decs' <- local (modifyFixEnv (`M.union` newenv)) $ opParserDecls decs
                return $ dec : decs'
        FixityD fix op -> (dec :) <$> local (modifyFixEnv $ extend (unLoc op) fix) (opParserDecls decs)
        ModuleD id (L _ (Module moddecs)) -> do
                moddecs' <- opParserDecls moddecs
                x <- do
                        opParserDecls moddecs
                return (dec : decs')
        DataD{} -> (dec :) <$> opParserDecls decs
        FuncSigD{} -> (dec :) <$> opParserDecls decs
        FuncD id pats body -> do
                body' <- opParser body
                (L sp (FuncD id pats body') :) <$> opParserDecls decs-}

--decs' <- local (`M.union` newenv) $ opParserDecl decs
--return $ d : dec'
opParserDecl :: (MonadState env m, HasFixEnv env, MonadThrow m) => LDecl -> m LDecl
opParserDecl dec = case unLoc dec of
        OpenD path -> do
                env <- gets getFixEnv
                let newenv = access path env
                modify $ modifyFixEnv (`M.union` newenv)
                return dec
        FixityD fix op -> do
                modify $ modifyFixEnv $ extend (unLoc op) fix
                return dec
        ModuleD id (L sp1 (Module moddecs)) -> do
                moddecs' <- mapM opParserDecl moddecs
                return dec{unLoc = ModuleD id (L sp1 (Module moddecs'))}
        DataD{} -> return dec
        FuncSigD{} -> return dec
        FuncD id pats body -> do
                env <- get
                body' <- runReaderT (opParser body) env
                return dec{unLoc = FuncD id pats body'}