{-# LANGUAGE FlexibleInstances #-}

module Plato.Common.Fixity (
        FixDir (..),
        maxPrec,
        minPrec,
        Fixity (..),
        FixEnv,
        FixBind (..),
        FixEnvManager (..),
        HasFixEnv (..),
) where

import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Path

----------------------------------------------------------------
-- Precedence
----------------------------------------------------------------
type FixPrec = Int

maxPrec :: Int
maxPrec = 9

minPrec :: Int
minPrec = 0

----------------------------------------------------------------
-- Fixity
----------------------------------------------------------------
data FixDir = Leftfix | Rightfix | Nonfix deriving (Eq, Show)

instance Pretty FixDir where
        pretty Leftfix = "infixl"
        pretty Rightfix = "infixr"
        pretty Nonfix = "infix"

data Fixity = Fixity FixPrec FixDir deriving (Eq, Show)

defaultFixity :: Fixity
defaultFixity = Fixity maxPrec Leftfix

----------------------------------------------------------------
-- FixityEnv
----------------------------------------------------------------
type FixEnv = NameMap FixBind

data FixBind = FixEnv FixEnv | FixBind Fixity

class FixEnvManager a where
        extend :: Name -> a -> FixEnv -> FixEnv
        access :: Path -> FixEnv -> a

access' :: Path -> FixEnv -> Maybe FixBind
access' (PIdent id) env = M.lookup (nameIdent id) env
access' (PDot root field) env = M.lookup (unLoc field) (access root env)

instance FixEnvManager Fixity where
        extend x fix = M.insert x (FixBind fix)
        access path env = case access' path env of
                Just (FixBind fix) -> fix
                _ -> defaultFixity

instance FixEnvManager FixEnv where
        extend x env = M.insert x (FixEnv env)
        access path env = case access' path env of
                Just (FixEnv env') -> env'
                _ -> M.empty

class HasFixEnv a where
        getFixEnv :: a -> FixEnv
        modifyFixEnv :: (FixEnv -> FixEnv) -> a -> a

instance HasFixEnv FixEnv where
        getFixEnv = id
        modifyFixEnv = id