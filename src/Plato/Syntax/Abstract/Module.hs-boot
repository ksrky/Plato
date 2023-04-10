{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module Plato.Syntax.Abstract.Module where

import qualified Data.Kind
import Prettyprinter

type role Decl nominal
data Decl :: Data.Kind.Type -> Data.Kind.Type

instance Eq (Decl a)
instance Show (Decl a)
instance Pretty (Decl a)