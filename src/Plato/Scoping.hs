{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Plato.Scoping (scoping) where

import Control.Exception.Safe
import Control.Monad.Reader.Class
import qualified Data.Map.Strict as M
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

instance HasScope Scope where
        getScope = id
        modifyScope = id

class Scoping a where
        scoping :: (MonadReader env m, HasScope env, MonadThrow m) => a -> m a

instance Scoping a => Scoping (Located a) where
        scoping (L sp x) = L sp <$> scoping x

instance Scoping a => Scoping [Located a] where
        scoping xs = mapM scoping xs

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
                                        (pats,) <$> scoping body
                                )
                                alts
        scoping (LetE decs body) = LetE <$> mapM scoping decs <*> scoping body
        scoping (CaseE match alts) =
                CaseE <$> scoping match
                        <*> mapM (\(pat, body) -> (pat,) <$> scoping body) alts
        scoping (FactorE exp) = FactorE <$> scoping exp

instance Scoping Pat where
        scoping (ConP con pats) = ConP <$> scoping con <*> mapM scoping pats
        scoping (VarP var) = VarP <$> scoping var
        scoping WildP = return WildP

instance Scoping Type where
        scoping (VarT var) = VarT <$> scoping var
        scoping (ConT con) = ConT <$> scoping con
        scoping (AppT fun arg) = AppT <$> scoping fun <*> scoping arg
        scoping (ArrT arg res) = ArrT <$> scoping arg <*> scoping res
        scoping (AllT qnts body) = do
                paramNamesUnique qnts
                AllT qnts <$> scoping body

instance Scoping Decl where
        scoping (OpenD path) = OpenD <$> scoping path
        scoping (FixityD fix id) = FixityD fix <$> scoping id
        scoping (ModuleD id mod) = ModuleD id <$> scoping mod
        scoping (DataD id params fields) = do
                paramNamesUnique params
                DataD id params <$> mapM (\(con, body) -> (con,) <$> scoping body) fields
        scoping (FuncSigD id ty) = FuncSigD id <$> scoping ty
        scoping (FuncD id params exp) = do
                paramPatsUnique params
                FuncD <$> scoping id <*> mapM scoping params <*> scoping exp

instance Scoping [Decl] where
        scoping decs = do
                let modids = [id | ModuleD id _ <- decs]
                let dataids = [id | DataD id _ _ <- decs]
                let funids = [id | FuncSigD id _ <- decs]
                defNamesUnique [id | ModuleD id _ <- decs]
                defNamesUnique [id | DataD id _ _ <- decs]
                defNamesUnique [id | FuncSigD id _ <- decs]
                dataConUnique $ [map fst fields | DataD _ _ fields <- decs]
                let extend = \sc -> foldr (\id -> M.insert (nameIdent id) id) sc (modids ++ dataids ++ funids)
                local (modifyScope extend) $ mapM scoping decs

scopingDecls :: (MonadReader env m, MonadThrow m, HasScope env) => [LDecl] -> m ([LDecl], Scope)
scopingDecls decs = do
        let modids = [id | L _ (ModuleD id _) <- decs]
        let dataids = [id | L _ (DataD id _ _) <- decs]
        let funids = [id | L _ (FuncSigD id _) <- decs]
        defNamesUnique modids
        defNamesUnique dataids
        defNamesUnique funids
        dataConUnique $ [map fst fields | L _ (DataD _ _ fields) <- decs]
        sc <- asks getScope
        let sc' = foldr (\id -> M.insert (nameIdent id) id) sc (modids ++ dataids ++ funids)
        (,sc') <$> local (modifyScope $ const sc') (mapM scoping decs)

instance Scoping Module where
        scoping (Module decs) = Module <$> scoping decs

scopingTopDecls :: (MonadReader env m, MonadThrow m, HasScope env) => [LTopDecl] -> m [LTopDecl]
scopingTopDecls tdecs = do
        imps <- sequence [L sp <$> (Import isopen <$> scoping path) | L sp (Import isopen path) <- tdecs]
        (decs, _) <- scopingDecls [d | L _ (Decl d) <- tdecs]
        evls <- sequence [L sp <$> (Eval <$> scoping e) | L sp (Eval e) <- tdecs]
        return $ imps {- ++ decs-} ++ evls