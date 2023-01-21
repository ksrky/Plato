{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.TypeCheck.Monad where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter
import Prettyprinter.Render.String

import Control.Exception.Safe
import Plato.Syntax.Typing
import Plato.Types.Error
import Plato.Types.Location

-- | TcEnv
data TcEnv = TcEnv
        { tc_env :: TyEnv
        , tc_uniq :: IORef Uniq
        , tc_loc :: IORef Span -- error location
        }

-- | Tc monad
newtype Tc m a = Tc {runTc :: TcEnv -> m a}

instance Monad m => Functor (Tc m) where
        fmap f (Tc m) = Tc $ \env -> do
                x <- m env
                return (f x)

instance Monad m => Applicative (Tc m) where
        pure x = Tc (\_ -> return x)
        f <*> x = Tc $ \env -> do
                f' <- runTc f env
                runTc (fmap f' x) env

instance Monad m => Monad (Tc m) where
        m >>= k = Tc $ \env -> do
                v <- runTc m env
                runTc (k v) env

instance MonadFail m => MonadFail (Tc m) where
        fail s = Tc $ \_ -> fail s

instance MonadTrans Tc where
        lift m = Tc (const m)

instance Monad m => MonadReader TyEnv (Tc m) where
        ask = Tc (return . tc_env)
        local f (Tc m) = Tc (\env -> m env{tc_env = f (tc_env env)})

failTc :: MonadFail m => Doc ann -> Tc m a
failTc doc = fail $ renderString $ layoutPretty defaultLayoutOptions doc

throwTc :: MonadThrow m => Span -> Doc ann -> Tc m a
throwTc sp doc = lift $ throwLocErr sp doc

-- | IORef
newTcRef :: MonadIO m => a -> Tc m (IORef a)
newTcRef v = lift (liftIO $ newIORef v)

readTcRef :: MonadIO m => IORef a -> Tc m a
readTcRef r = lift (liftIO $ readIORef r)

writeTcRef :: MonadIO m => IORef a -> a -> Tc m ()
writeTcRef r v = lift (liftIO $ writeIORef r v)

-- | type variable generation
newTyVar :: MonadIO m => Tc m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: MonadIO m => TyVar -> Tc m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUniq

newMetaTv :: MonadIO m => Tc m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newTcRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Tc m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readTcRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Tc m ()
writeMetaTv (MetaTv _ ref) ty = writeTcRef ref (Just ty)

newUniq :: MonadIO m => Tc m Uniq
newUniq = Tc $ \env -> do
        let ref = tc_uniq env
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- Writeing error location
readErrLoc :: MonadIO m => Tc m Span
readErrLoc = Tc $ \env -> do
        let ref = tc_loc env
        liftIO $ readIORef ref

writeErrLoc :: MonadIO m => Span -> Tc m ()
writeErrLoc sp = Tc $ \env -> do
        let ref = tc_loc env
        liftIO $ writeIORef ref sp

-- | Environment management
emptyEnv :: MonadIO m => m TcEnv
emptyEnv = do
        uniq_ref <- liftIO $ newIORef 0
        loc_ref <- liftIO $ newIORef NoSpan
        return $ TcEnv M.empty uniq_ref loc_ref

extendEnv :: Monad m => LName -> Sigma -> Tc m a -> Tc m a
extendEnv x ty = local (M.insert (unLoc x) ty)

extendEnvList :: Monad m => [(LName, Located Sigma)] -> Tc m a -> Tc m a
extendEnvList binds m = foldl (\m (x, ty) -> extendEnv x (unLoc ty) m) m binds

lookupEnv :: MonadThrow m => LName -> Tc m Sigma
lookupEnv (L sp x) = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> throwTc sp $ hsep ["Not in scope ", squotes $ pretty x]