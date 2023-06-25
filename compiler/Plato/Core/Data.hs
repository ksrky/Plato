module Plato.Core.Data where

import Control.Monad.IO.Class
import GHC.IORef
import Plato.Common.Ident
import Plato.Syntax.Core

{-type CoreEnv = EnvEntries

class HasCoreEnv e where
        getCoreEnv :: MonadIO m => e -> m CoreEnv
        setCoreEnv :: MonadIO m => e -> CoreEnv -> m ()

instance HasCoreEnv (IORef CoreEnv) where
        getCoreEnv = liftIO . readIORef
        setCoreEnv ref = liftIO . writeIORef ref

extE :: (MonadReader e m, HasCoreEnv e, MonadIO m) => PrtInfo -> m Index
extE fi = do
        env <- getCoreEnv =<< ask
        undefined

setE :: (MonadReader e m, HasCoreEnv e, MonadIO m) => Index -> EnvEntry -> m ()
setE i v = do
        env <- getCoreEnv =<< ask
        let fi = snd (env !! i)
        flip setCoreEnv (set env i (v, fi)) =<< ask

getE :: (MonadReader e m, HasCoreEnv e, MonadIO m) => Index -> m EnvEntry
getE i = fst . (!! i) <$> (getCoreEnv =<< ask)
-}
class Env e where
        emptyE :: MonadIO m => m e
        extendE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: MonadIO m => Index -> e -> m EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()

set :: [a] -> Int -> a -> [a]
set [] _ _ = error "list is empty"
set (_ : as) 0 b = b : as
set (a : as) i b = a : set as (i - 1) b

instance Env (IORef EnvEntries) where
        emptyE = liftIO $ newIORef []
        extendE fi ref = do
                env <- liftIO $ readIORef ref
                let i = length env
                liftIO $ writeIORef ref (env ++ [(Index i, fi)])
                return i
        getE i ref = do
                env <- liftIO $ readIORef ref
                return $ fst $ env !! i
        setE i v ref = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (set env i (v, snd (env !! i)))

type Index = Int

newtype Scope = Scope [(Ident, (Index, Maybe (Clos Type)))] deriving (Eq, Show)

type Clos a = (a, Scope)

newtype Boxed = Boxed (Clos Term) deriving (Eq, Show)

data Val
        = Ne Ne
        | VType
        | VQ PiSigma (Clos (Type, Bind Type))
        | VLift (Clos Type)
        | VLam (Bind (Clos Term))
        | VPair (Clos (Term, Term))
        | VEnum [Label]
        | VLabel Label
        | VBox Boxed
        | VRec (Clos Type)
        | VFold (Clos Term)
        deriving (Eq, Show)

-- | Neutral terms.
data Ne
        = NVar Index
        | Ne :.. (Clos Term)
        | NSplit Ne (Bind (Bind (Clos Term)))
        | NCase Ne (Clos [(Label, Term)])
        | NForce Ne
        | NUnfold Ne (Bind (Clos Term))
        deriving (Eq, Show)

data EnvEntry
        = Index Index
        | Closure (Clos Term)
        deriving (Show)

type EnvEntries = [(EnvEntry, PrtInfo)]

data PrtInfo = PrtInfo
        { name :: Ident
        , expand :: Bool
        }
