{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.KindInfer.Monad where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Syntax.Typing
import Plato.Types.Error
import Plato.Types.Location

-- | Kind environment
data KiEnv = KiEnv
        { ki_env :: KnEnv
        , ki_uniq :: IORef Uniq
        , ki_loc :: IORef Span
        }

-- | Inference Monad
newtype Ki m a = Ki {runKi :: KiEnv -> m a}

instance Monad m => Functor (Ki m) where
        fmap f (Ki m) = Ki $ \env -> do
                x <- m env
                return $ f x

instance Monad m => Applicative (Ki m) where
        pure x = Ki (\_ -> return x)
        f <*> x = Ki $ \env -> do
                f' <- runKi f env
                runKi (fmap f' x) env

instance Monad m => Monad (Ki m) where
        m >>= k = Ki $ \env -> do
                v <- runKi m env
                runKi (k v) env

instance MonadTrans Ki where
        lift m = Ki (const m)

instance Monad m => MonadReader KnEnv (Ki m) where
        ask = Ki (return . ki_env)
        local f (Ki m) = Ki (\env -> m env{ki_env = f (ki_env env)})

throwKi :: MonadThrow m => Span -> Doc ann -> Ki m a
throwKi sp doc = lift $ throwLocErr sp doc

-- | IORef
newKiRef :: MonadIO m => a -> Ki m (IORef a)
newKiRef v = lift (liftIO $ newIORef v)

readKiRef :: MonadIO m => IORef a -> Ki m a
readKiRef r = lift (liftIO $ readIORef r)

writeKiRef :: MonadIO m => IORef a -> a -> Ki m ()
writeKiRef r v = lift (liftIO $ writeIORef r v)

-- | kind variable generation
newKnVar :: MonadIO m => Ki m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: MonadIO m => Ki m MetaKv
newMetaKv = MetaKv <$> newUniq <*> newKiRef Nothing

readMetaKv :: MonadIO m => MetaKv -> Ki m (Maybe Kind)
readMetaKv (MetaKv _ ref) = readKiRef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> Ki m ()
writeMetaKv (MetaKv _ ref) ty = writeKiRef ref (Just ty)

newUniq :: MonadIO m => Ki m Uniq
newUniq = Ki $ \env -> do
        let ref = ki_uniq env
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- Writeing error location
readErrLoc :: MonadIO m => Ki m Span
readErrLoc = Ki $ \env -> do
        let ref = ki_loc env
        liftIO $ readIORef ref

writeErrLoc :: MonadIO m => Span -> Ki m ()
writeErrLoc sp = Ki $ \env -> do
        let ref = ki_loc env
        liftIO $ writeIORef ref sp

-- | Environment management
emptyEnv :: MonadIO m => m KiEnv
emptyEnv = do
        uniq_ref <- liftIO $ newIORef 0
        loc_ref <- liftIO $ newIORef NoSpan
        return $ KiEnv M.empty uniq_ref loc_ref

extendEnv :: Monad m => LName -> Kind -> Ki m a -> Ki m a
extendEnv x ty = local (M.insert (unLoc x) ty)

extendEnvList :: Monad m => [(LName, Kind)] -> Ki m a -> Ki m a
extendEnvList binds m = foldl (\m (x, ty) -> extendEnv x ty m) m binds

lookupEnv :: MonadThrow m => LName -> Ki m Kind
lookupEnv (L sp x) = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> throwKi sp $ hsep ["Not in scope ", squotes $ pretty x]