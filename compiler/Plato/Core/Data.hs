module Plato.Core.Data where

import Control.Monad.IO.Class
import GHC.IORef
import Plato.Common.Ident
import Plato.Syntax.Core

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
        | VQ PiSigma (Clos (Bind Type)) (Clos Type)
        | VLift (Clos Type)
        | VLam (Bind (Clos Term))
        | VPair (Clos Term) (Clos Term)
        | VEnum [Label]
        | VLabel Label
        | VBox Boxed
        | VRec (Clos Type)
        | VFold (Clos Term)
        deriving (Eq, Show)

instance Pretty (Clos t) where
        pretty (t, _) = pretty t

instance Pretty Boxed where
        pretty (Boxed t) = brackets $ pretty t

instance Pretty Val where
        pretty (Ne ne) = pretty ne
        pretty VType = "*"
        pretty (VQ Pi bind ty) = hsep [parens (pretty bind), "->", pretty ty]
        pretty (VQ Sigma bind ty) = hsep [parens (pretty bind), "*", pretty ty]
        pretty (VLam bind t) = hsep ["\\", pretty bind, dot, pretty t]
        pretty (VPair t u) = parens (pretty t <> comma <+> pretty u)
        pretty (VEnum labs) = braces $ concatWith (surround (comma <> space)) (map pretty labs)
        pretty (VLabel lab) = "`" <> pretty lab
        pretty (VBox box) = pretty box
        pretty (VRec ty) = "Rec" <+> pretty ty
        pretty (VFold t) = "fold" <+> pretty t

instance Pretty Ne where
        pretty (NVar i) = pretty i
        pretty (ne :.. t) = hsep [pretty ne, ":..", pretty t]
        pretty (NSplit ne (x, y, t)) = "<not yet>" 
        pretty (NCase ne lts) = "<not yet>"
        pretty (NForce ne) = "<not yet>"
        pretty (NUnfold ne bind) = "<not yet>"

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
