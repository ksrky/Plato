{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Plato.Scoping (scopingProgram) where

import Control.Exception.Safe
import Control.Monad.Reader.Class
import Control.Monad.State
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Path
import Plato.Scoping.Restrictions
import Plato.Syntax.Parsing

type Scope = M.Map Name Ident

class HasScope a where
        getScope :: a -> Scope
        modifyScope :: (Scope -> Scope) -> a -> a
        extendScope :: Ident -> a -> a
        extendScope id = modifyScope (M.insert (nameIdent id) id)

instance HasScope Scope where
        getScope = id
        modifyScope = id

class Scoping a where
        scoping :: (MonadReader env m, HasScope env, MonadThrow m) => a -> m a

instance Scoping a => Scoping (Located a) where
        scoping (L sp x) = L sp <$> scoping x

instance Scoping a => Scoping [a] where
        scoping = mapM scoping

instance Scoping Ident where
        scoping id = do
                sc <- asks getScope
                case M.lookup (nameIdent id) sc of
                        Just id' -> return id'
                        Nothing ->
                                throwLocErr (getLoc id) $
                                        hsep ["Not in scope ", squotes $ pretty id]

instance Scoping Path where
        scoping (PIdent id) = do
                id' <- scoping id
                return $ PIdent id'
        scoping (PDot root field) = do
                root' <- scoping root
                return $ PDot root' field

instance Scoping Expr where
        scoping (VarE path) = VarE <$> scoping path
        scoping (AppE fun arg) = AppE <$> scoping fun <*> scoping arg
        scoping (OpE lhs op rhs) = OpE <$> scoping lhs <*> scoping op <*> scoping rhs
        scoping (LamE alts) =
                LamE
                        <$> mapM
                                ( \(pats, body) -> do
                                        paramPatsUnique pats
                                        (,) <$> scoping pats <*> scoping body
                                )
                                alts
        scoping (LetE decs body) = do
                (decs', env') <- runStateT (checkDecls decs) =<< ask
                body' <- local (const env') (scoping body)
                return $ LetE decs' body'
        scoping (CaseE match alts) =
                CaseE
                        <$> scoping match
                        <*> mapM (\(pat, body) -> (pat,) <$> scoping body) alts
        scoping (FactorE exp) = FactorE <$> scoping exp

instance Scoping Pat where
        scoping (ConP con pats) = ConP <$> scoping con <*> mapM scoping pats
        scoping (VarP var) = return $ VarP var
        scoping WildP = return WildP

instance Scoping Type where
        scoping (VarT var) = VarT <$> scoping var
        scoping (ConT con) = ConT <$> scoping con
        scoping (AppT fun arg) = AppT <$> scoping fun <*> scoping arg
        scoping (ArrT arg res) = ArrT <$> scoping arg <*> scoping res
        scoping (AllT qnts body) = do
                paramNamesUnique qnts
                AllT qnts <$> local (\env -> foldr extendScope env qnts) (scoping body)

instance Scoping Decl where
        scoping (OpenD path) = OpenD <$> scoping path
        scoping (FixityD fix x) = return $ FixityD fix x
        scoping (ModuleD id mod) = do
                -- modify $ extendScope id
                ModuleD id <$> scoping mod
        scoping (DataD id params fields) = do
                paramNamesUnique params
                DataD id params <$> mapM (\(con, body) -> (con,) <$> scoping body) fields
        scoping (FuncSigD id ty) = FuncSigD id <$> scoping ty
        scoping (FuncD id params exp) = do
                paramPatsUnique params
                FuncD <$> scoping id <*> mapM scoping params <*> scoping exp

checkDecls ::
        (MonadState env m, MonadReader env m, MonadThrow m, HasScope env) =>
        [LDecl] ->
        m [LDecl]
checkDecls decs = do
        let modids = [id | L _ (ModuleD id _) <- decs]
        let dataids = [id | L _ (DataD id _ _) <- decs]
        let funids = [id | L _ (FuncSigD id _) <- decs]
        defNamesUnique modids
        defNamesUnique dataids
        defNamesUnique funids
        dataConUnique $ [map fst fields | L _ (DataD _ _ fields) <- decs]
        sc <- asks getScope
        let sc' = foldr extendScope sc (modids ++ dataids ++ funids)
        modify $ modifyScope $ const sc'
        local (modifyScope $ const sc') (scopingDecls decs)

scopingDecls :: (MonadReader env m, HasScope env, MonadThrow m) => [LDecl] -> m [LDecl]
scopingDecls [] = return []
scopingDecls (dec : decs) = case unLoc dec of
        OpenD path -> undefined {-do
                                path' <- scoping path
                                sc <- asks getScope
                                return $ local (extendScope) $ (dec{unLoc = OpenD path'} : decs) -}
        FixityD{} -> (dec :) <$> scopingDecls decs
        ModuleD id mod -> do
                local (extendScope id) $ do
                        mod' <- scoping mod
                        decs' <- local (extendScope id) (scopingDecls decs)
                        return $ dec{unLoc = ModuleD id mod'} : decs'
        DataD id params constrs -> do
                paramNamesUnique params
                local (extendScope id) $ do
                        constrs' <-
                                local (\env -> foldr extendScope env params) $
                                        mapM (\(con, ty) -> (con,) <$> scoping ty) constrs
                        (dec{unLoc = DataD id params constrs'} :) <$> scopingDecls decs
        FuncSigD id ty -> local (extendScope id) $ do
                ty' <- scoping ty
                (dec{unLoc = FuncSigD id ty'} :) <$> scopingDecls decs
        FuncD id pats body -> do
                paramPatsUnique pats
                d <- FuncD <$> scoping id <*> mapM scoping pats <*> scoping body
                (dec{unLoc = d} :) <$> scopingDecls decs

instance Scoping Module where
        scoping (Module decs) = do
                decs' <- evalStateT (checkDecls decs) =<< ask
                return $ Module decs'

scopingProgram :: (MonadState env m, MonadReader env m, MonadThrow m, HasScope env) => Program -> m Program
scopingProgram tdecs = do
        imps <- sequence [L sp <$> (Import <$> scoping path) | L sp (Import path) <- tdecs]
        let (fs, decs) = unzip [(L sp . Decl, d) | L sp (Decl d) <- tdecs]
        (decs', env) <- runStateT (checkDecls decs) =<< ask
        evls <- local (const env) $ sequence [L sp <$> (Eval <$> scoping e) | L sp (Eval e) <- tdecs]
        return (imps ++ zipWith id fs decs' ++ evls)