module Plato.Parsing.Rename where

import Plato.Syntax.Parsing

import Plato.Types.Error
import Plato.Types.Fixity
import Plato.Types.Location
import Plato.Types.Name
import Plato.Types.Name.Global
import Plato.Types.Name.Reader

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Data.List (find)
import qualified Data.Map.Strict as M
import Prettyprinter

data REnv = REnv
        { r_glbenv :: GlbEnv
        , r_level :: Int
        }

initREnv :: GlbEnv -> REnv
initREnv glbenv = REnv{r_glbenv = glbenv, r_level = 0}

----------------------------------------------------------------
-- Renaming
----------------------------------------------------------------
class Rename f where
        rename :: MonadThrow m => f RdrName -> ReaderT REnv m (f GlbName)

instance Rename Located where
        rename (L sp (Unqual n)) = do
                env <- asks r_glbenv
                L sp <$> lookupGlbEnv env (L sp n)
        rename (L sp (Qual modn n)) = do
                env <- asks r_glbenv
                unless (n `M.member` env) $
                        throwLocErr sp $
                                hsep ["No module named", squotes $ pretty n, "is imported"]
                return $ L sp $ toplevelName modn (L sp n)

instance Rename Expr where
        rename (VarE var) = VarE <$> rename var
        rename (AppE fun arg) = AppE <$> rename `traverse` fun <*> rename `traverse` arg
        rename (OpE lhs op rhs) = OpE <$> rename `traverse` lhs <*> rename op <*> rename `traverse` rhs
        rename (LamE args body) = do
                checkUniqArgs args
                local (\env -> env{r_glbenv = extendEnvListLocal args (r_glbenv env)}) $
                        LamE args <$> rename `traverse` body
        rename (LetE decs body) = do
                let names = [var | L _ (FuncTyD var _) <- decs]
                checkUniqNames names
                local (\env -> env{r_glbenv = extendEnvListLocal names (r_glbenv env)}) $ do
                        decs' <-
                                local (\env -> env{r_level = r_level env + 1}) $
                                        mapM (rename `traverse`) decs
                        LetE decs' <$> rename `traverse` body
        rename (CaseE match alts) = do
                match' <- rename `traverse` match
                alts' <- forM alts $ \(pat, body) -> do
                        pat' <- rename `traverse` pat
                        let args = getPatArgs (unLoc pat)
                        checkUniqArgs args
                        body' <-
                                local (\env -> env{r_glbenv = extendEnvListLocal args (r_glbenv env)}) $
                                        rename `traverse` body
                        return (pat', body')
                return $ CaseE match' alts'
            where
                getPatArgs :: Pat RdrName -> [LArg]
                getPatArgs (ConP _ pats) = concatMap (getPatArgs . unLoc) pats
                getPatArgs (VarP var) = [var]
                getPatArgs WildP = []
        rename (FactorE exp) = FactorE <$> rename `traverse` exp

instance Rename Pat where
        rename (ConP con pats) = ConP <$> rename con <*> mapM (rename `traverse`) pats
        rename (VarP var) = return $ VarP var
        rename WildP = return WildP

instance Rename Type where
        rename (VarT var) = return $ VarT var
        rename (ConT con) = ConT <$> rename con --temp
        rename (AppT funty argty) = AppT <$> rename `traverse` funty <*> rename `traverse` argty
        rename (ArrT argty resty) = ArrT <$> rename `traverse` argty <*> rename `traverse` resty
        rename (AllT args bodyty) = do
                checkUniqArgs args
                local (\env -> env{r_glbenv = extendEnvListLocal args (r_glbenv env)}) $
                        AllT args <$> rename `traverse` bodyty

instance Rename Decl where
        rename (FuncD var args body) = do
                checkUniqArgs args
                local (\env -> env{r_glbenv = extendEnvListLocal args (r_glbenv env)}) $
                        FuncD var args <$> rename `traverse` body
        rename (FuncTyD var body_ty) = FuncTyD var <$> rename `traverse` body_ty
        rename (FixityD fix prec op) = return $ FixityD fix prec op

instance Rename TopDecl where
        rename (DataD con args fields) = do
                checkUniqArgs args
                fields' <- forM fields $ \(dcon, tys) -> do
                        tys' <- mapM (rename `traverse`) tys
                        return (dcon, tys')
                return $ DataD con args fields'
        rename (Decl dec) = Decl <$> rename `traverse` dec
        rename (Eval exp) = Eval <$> rename `traverse` exp

renameProgram :: MonadThrow m => Program RdrName -> GlbEnv -> m (Program GlbName, GlbEnv)
renameProgram (Program modn imps topds) glbenv = do
        names <- forM topds $ \(L _ topd) -> case topd of
                DataD con _ fields -> return $ con : map fst fields
                Decl (L _ (FuncTyD var _)) -> return [var]
                _ -> return []
        checkUniqNames (concat names)
        let glbenv' = extendEnvListExt modn (concat names) glbenv
        topds' <- mapM (rename `traverse`) topds `runReaderT` initREnv glbenv'
        return (Program modn imps topds', glbenv')

renameFixityEnv :: GlbEnv -> FixityEnv Name -> FixityEnv GlbName
renameFixityEnv glbenv = M.mapKeys $ \n -> case M.lookup n glbenv of
        Just glbn -> glbn
        Nothing -> localName (noLoc n)

----------------------------------------------------------------
-- namesCheck
----------------------------------------------------------------
checkUniqNames :: MonadThrow m => [LName] -> m ()
checkUniqNames ns = loop ns
    where
        loop :: MonadThrow m => [LName] -> m ()
        loop [] = return ()
        loop (L _ n1 : ns) = case find ((n1 ==) . unLoc) ns of
                Just (L sp2 n2) ->
                        throwLocErr sp2 $
                                hsep
                                        [ "Multiple declarations for"
                                        , squotes $ pretty n2
                                        ]
                Nothing -> loop ns

checkUniqArgs :: MonadThrow m => [LArg] -> m ()
checkUniqArgs ns = case [(n1, sp1, sp2) | L sp1 n1 <- ns, L sp2 n2 <- ns, n1 == n2, sp1 /= sp2] of
        [] -> return ()
        (n, sp1, sp2) : _ ->
                throwLocErr (combineSpans sp1 sp2) $
                        hsep
                                [ "Conflicting definitions for"
                                , squotes $ pretty n
                                ]
