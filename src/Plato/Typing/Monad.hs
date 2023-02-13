{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Typing.Monad where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.IORef
import Prettyprinter
import Prettyprinter.Render.String

import Plato.Common.Error
import Plato.Common.Location 
import Plato.Syntax.Typing

data Context = Context
        { typenv :: TypEnv
        , uniq :: IORef Uniq
        , errloc :: IORef Span -- error location
        }

newtype Typ m a = Typ {runTyp :: Context -> m a}

instance Monad m => Functor (Typ m) where
        fmap f (Typ m) = Typ $ \ctx -> do
                x <- m ctx
                return (f x)

instance Monad m => Applicative (Typ m) where
        pure x = Typ (\_ -> return x)
        f <*> x = Typ $ \ctx -> do
                f' <- runTyp f ctx
                runTyp (fmap f' x) ctx

instance Monad m => Monad (Typ m) where
        m >>= k = Typ $ \ctx -> do
                v <- runTyp m ctx
                runTyp (k v) ctx

instance MonadFail m => MonadFail (Typ m) where
        fail s = Typ $ \_ -> fail s

instance MonadTrans Typ where
        lift m = Typ (const m)

instance Monad m => MonadReader TypEnv (Typ m) where
        ask = Typ (return . typenv)
        local f (Typ m) = Typ (\ctx -> m ctx{typenv = f (typenv ctx)})

asksM :: (TypEnv -> m a) -> Typ m a
asksM f = Typ (f . typenv)

failTyp :: MonadFail m => Doc ann -> Typ m a
failTyp doc = fail $ renderString $ layoutPretty defaultLayoutOptions doc

throwTyp :: MonadThrow m => Span -> Doc ann -> Typ m a
throwTyp sp doc = lift $ throwLocErr sp doc

-- | creating, reading and writing IORef
newTypRef :: MonadIO m => a -> Typ m (IORef a)
newTypRef v = lift (liftIO $ newIORef v)

readTypRef :: MonadIO m => IORef a -> Typ m a
readTypRef r = lift (liftIO $ readIORef r)

writeTypRef :: MonadIO m => IORef a -> a -> Typ m ()
writeTypRef r v = lift (liftIO $ writeIORef r v)

-- creating and rewriting Uniq
newUniq :: MonadIO m => Typ m Uniq
newUniq = Typ $ \ctx -> do
        let ref = uniq ctx
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- | type variable generation
newTyVar :: MonadIO m => Typ m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: MonadIO m => TyVar -> Typ m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUniq

newMetaTv :: MonadIO m => Typ m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newTypRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Typ m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readTypRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Typ m ()
writeMetaTv (MetaTv _ ref) ty = writeTypRef ref (Just ty)

-- | kind variable generation
newKnVar :: MonadIO m => Typ m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: MonadIO m => Typ m MetaKv
newMetaKv = MetaKv <$> newUniq <*> newTypRef Nothing

readMetaKv :: MonadIO m => MetaKv -> Typ m (Maybe Kind)
readMetaKv (MetaKv _ ref) = readTypRef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> Typ m ()
writeMetaKv (MetaKv _ ref) ty = writeTypRef ref (Just ty)

-- | writeing error location
readErrLoc :: MonadIO m => Typ m Span
readErrLoc = Typ $ \env -> do
        let ref = errloc env
        liftIO $ readIORef ref

writeErrLoc :: MonadIO m => Span -> Typ m ()
writeErrLoc sp = Typ $ \env -> do
        let ref = errloc env
        liftIO $ writeIORef ref sp

-- | Context management
initContext :: MonadIO m => TypEnv -> m Context
initContext typenv = do
        uniq <- liftIO $ newIORef 0
        errloc <- liftIO $ newIORef NoSpan
        return $ Context typenv uniq errloc