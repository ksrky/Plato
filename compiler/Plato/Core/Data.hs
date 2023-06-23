module Plato.Core.Data where

import Control.Monad.IO.Class
import Plato.Common.Ident
import Plato.Syntax.Core

class CoreEnv e where
        emptyE :: e
        extendE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: Index -> e -> EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()

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

data PrtInfo = PrtInfo
        { name :: Ident
        , expand :: Bool
        }
