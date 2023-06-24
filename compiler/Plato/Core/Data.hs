module Plato.Core.Data where

import Control.Monad.IO.Class
import Data.IORef

import Plato.Common.Ident
import Plato.Syntax.Core

class CoreEnv e where
        emptyE :: MonadIO m => m e
        extendE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: MonadIO m => Index -> e -> m EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()

instance CoreEnv (IORef EnvEntries) where
        {-emptyE = liftIO $ newIORef []
        extendE fi env = do
                let i = length env
                env ++ [(Id i, fi)]
                return i
        getE i ref = do
                env <- liftIO $ readIORef ref
                case env !! i of (v, _) -> return v
        setE i v ref = do
                env <- liftIO $ readIORef ref
                case env !! i of (_, fi) -> liftIO $ writeIORef (env !! i) (v, fi)-}
                
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
