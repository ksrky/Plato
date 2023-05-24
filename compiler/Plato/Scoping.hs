module Plato.Scoping (
        Scope,
        initScope,
        scoping,
        scopingDecl,
        scopingProgram,
) where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Scoping.SynRstrc
import Plato.Scoping.Utils
import Plato.Syntax.Parsing

type Scope = M.Map Name Ident

class HasScope a where
        getScope :: a -> Scope
        modifyScope :: (Scope -> Scope) -> a -> a
        extendScope :: Ident -> a -> a
        extendScope id = modifyScope (M.insert (nameIdent id) id)
        extendListScope :: [Ident] -> a -> a
        extendListScope ids env = foldr extendScope env ids

instance HasScope Scope where
        getScope = id
        modifyScope = id

initScope :: Scope
initScope = M.empty

-----------------------------------------------------------
-- Scoping
-----------------------------------------------------------
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

instance Scoping Expr where
        scoping (VarE var) = VarE <$> scoping var
        scoping (AppE fun arg) = AppE <$> scoping fun <*> scoping arg
        scoping (OpE left op right) = OpE <$> scoping left <*> pure op <*> scoping right
        scoping (LamE pats body) = do
                paramPatsUnique pats
                pats' <- scoping pats
                body' <- local (extendListScope (allIdentsFromPats pats)) $ scoping body
                return $ LamE pats' body'
        scoping (LetE decs body) = do
                (decs', env') <- runStateT (mapM scopingDecl decs) =<< ask
                decs'' <- bundleClauses decs'
                body' <- local (const env') (scoping body)
                return $ LetE decs'' body'
        scoping (FactorE exp) = FactorE <$> scoping exp

instance Scoping Pat where
        scoping (ConP con pats) = ConP <$> scoping con <*> mapM scoping pats
        scoping (VarP var) = return $ VarP var
        scoping WildP = return WildP

instance Scoping Type where
        scoping (VarT var) = VarT <$> scoping var
        scoping (ConT con) = ConT <$> scoping con
        scoping (ArrT arg res) = ArrT <$> scoping arg <*> scoping res
        scoping (AllT qnts body) = do
                paramNamesUnique qnts
                AllT qnts <$> local (extendListScope qnts) (scoping body)
        scoping (AppT fun arg) = AppT <$> scoping fun <*> scoping arg

instance Scoping Clause where
        scoping (pats, exp) = do
                -- Checking syntax restriction
                paramPatsUnique pats
                -- Checking scope
                pats' <- mapM scoping pats
                exp' <- local (extendListScope (allIdentsFromPats pats)) $ scoping exp
                return (pats', exp')

-----------------------------------------------------------
-- ScopingDecl
-----------------------------------------------------------
class ScopingDecl a where
        scopingDecl :: (MonadState env m, HasScope env, MonadThrow m) => a -> m a

instance ScopingDecl a => ScopingDecl (Located a) where
        scopingDecl (L sp x) = L sp <$> scopingDecl x

instance ScopingDecl a => ScopingDecl [a] where
        scopingDecl = mapM scopingDecl

instance ScopingDecl FunDecl where
        scopingDecl (FunSpec id ty) = do
                -- Checking scope
                env <- get
                ty' <- runReaderT (scoping ty) env
                -- Adding id to outer scope
                modify $ extendScope id
                -------------------------------------------
                return $ FunSpec id ty'
        scopingDecl (FunBind id clses) = do
                env <- get
                (`runReaderT` env) $ do
                        id' <- scoping id
                        clses' <- scoping clses
                        return $ FunBind id' clses'
        scopingDecl dec@FixDecl{} = return dec

instance ScopingDecl Decl where
        scopingDecl (DataD id params fields) = do
                -- Checking syntax restriction
                paramNamesUnique params
                dataConUnique $ map fst fields
                mapM_ (dataConType id) fields
                -- Checking scope
                env <- get
                fields' <-
                        (`runReaderT` env) $
                                local (extendListScope $ id : params) $
                                        mapM (\(con, body) -> (con,) <$> scoping body) fields
                -- Adding id to outer scope
                modify $ extendScope id
                -------------------------------------------
                return $ DataD id params fields'
        scopingDecl (FuncD fundec) = FuncD <$> scopingDecl fundec

scopingProgram :: MonadThrow m => Program -> m Program
scopingProgram tdecs = do
        (tdecs', _) <- runStateT (mapM scopingDecl tdecs) initScope
        return tdecs'