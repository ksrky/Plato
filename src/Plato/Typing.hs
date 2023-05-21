module Plato.Typing (typingProgram) where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Monad
import Plato.Typing.Tc

typing ::
        (MonadState env m, HasTypEnv env, HasUniq env, MonadThrow m, MonadIO m) =>
        Decl ->
        m Decl
typing (SpecDecl (TypSpec id kn)) = do
        modify (modifyEnv $ extend id kn)
        return $ SpecDecl (TypSpec id kn)
typing (SpecDecl (ValSpec id ty)) = do
        runReaderT (checkKindStar ty) =<< get
        modify (modifyEnv $ extend id ty)
        return $ SpecDecl (ValSpec id ty)
typing (BindDecl (DatBind id params constrs)) = do
        let extendEnv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        _ <- runReaderT (local (modifyEnv extendEnv) $ mapM (checkKindStar . snd) constrs) =<< get
        modify (modifyEnv $ extendList $ map (\(con, ty) -> (con, AllT params ty)) constrs)
        return $ BindDecl (DatBind id params constrs)
typing (BindDecl (TypBind id ty)) = do
        kn <- find id =<< getEnv =<< get -- tmp: zonking
        runReaderT (checkKind ty kn) =<< get
        return $ BindDecl (TypBind id ty)
typing (BindDecl (ValBind id exp)) = do
        ty <- find id =<< getEnv =<< get
        exp' <- runReaderT (checkType exp ty) =<< get
        return $ BindDecl (ValBind id exp')

typingProgram :: (PlatoMonad m, MonadThrow m) => Program -> m Program
typingProgram (decs, exps) = do
        ctx <- initContext
        (decs', ctx') <- runStateT (mapM typing decs) ctx
        exptys <- runReaderT (mapM inferType exps) ctx'
        return (decs', map fst exptys)