module Plato.Core.Data where

import Control.Monad.IO.Class
import GHC.IORef
import Prettyprinter

import Plato.Common.Ident
import Plato.Syntax.Core

class Env e where
        extendE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: MonadIO m => Index -> e -> m EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()
        prtE :: MonadIO m => Index -> e -> m PrtInfo

set :: [a] -> Int -> a -> [a]
set [] _ _ = error "list is empty"
set (_ : as) 0 b = b : as
set (a : as) i b = a : set as (i - 1) b

instance Env (IORef EnvEntries) where
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
        prtE i ref = do
                env <- liftIO $ readIORef ref
                return $ snd $ env !! i

type Index = Int

newtype Scope = Scope [(Ident, (Index, Maybe (Clos Type)))] deriving (Eq, Show)

type Clos a = (a, Scope)

newtype Boxed = Boxed (Clos Term) deriving (Eq, Show)

data Val
        = Ne Ne
        | VType
        | VQ PiSigma (Clos (Bind Type, Type))
        | VLift (Clos Type)
        | VLam (Clos (Bind Type, Term))
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

instance Pretty Boxed where
        pretty (Boxed (t, _)) = brackets $ pretty t

instance Pretty Val where
        pretty (Ne ne) = pretty ne
        pretty VType = "*"
        pretty (VQ Pi ((bind, ty), _)) = hsep [parens (prettyBind bind), "->", pretty ty]
        pretty (VQ Sigma ((bind, ty), _)) = hsep [parens (prettyBind bind), "*", pretty ty]
        pretty (VLift (ty, _)) = "^" <> pretty ty
        pretty (VLam (((id, _), t), _)) = hsep ["\\", prettyId id, dot, pretty t]
        pretty (VPair ((t, u), _)) = parens (pretty t <> comma <+> pretty u)
        pretty (VEnum labs) = braces $ concatWith (surround (comma <> space)) (map pretty labs)
        pretty (VLabel lab) = "`" <> pretty lab
        pretty (VBox box) = pretty box
        pretty (VRec (ty, _)) = "Rec" <+> pretty ty
        pretty (VFold (t, _)) = "fold" <+> pretty t

instance Pretty Ne where
        pretty (NVar i) = pretty i
        pretty (ne :.. (t, _)) = hsep [pretty ne, ":..", pretty t]
        pretty (NSplit ne (x, (y, (t, _)))) = hsep ["split", pretty ne, "with", braces (hsep [prettyId x, comma, prettyId y]), "->", pretty t]
        pretty (NCase ne (lts, _)) =
                hsep
                        [ "case"
                        , pretty ne
                        , braces $
                                concatWith
                                        (surround (semi <> space))
                                        (map (\(l, t) -> hsep [pretty l, "->", pretty t]) lts)
                        ]
        pretty (NForce ne) = "!" <> pretty ne
        pretty (NUnfold ne (x, (t, _))) = hsep ["unfold", pretty ne, "as", prettyId x, "->", pretty t]
