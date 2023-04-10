{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Plato.Syntax.Abstract.Kind where

import Data.IORef (IORef)
import qualified Data.Kind
import Prettyprinter

import Plato.Common.Global
import Plato.Common.Path
import Plato.Syntax.Abstract.Classes

----------------------------------------------------------------
-- Datas
----------------------------------------------------------------
data Kind :: Data.Kind.Type -> Data.Kind.Type where
        StarK ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                Kind a
        ArrK ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                Kind a ->
                Kind a ->
                Kind a
        MetaK ::
                CTyping a =>
                MetaKv a ->
                Kind a

data MetaKv :: Data.Kind.Type -> Data.Kind.Type where
        MetaKv ::
                CTyping a =>
                Unique ->
                IORef (Maybe (Kind a)) ->
                MetaKv a

----------------------------------------------------------------
-- Class instances
----------------------------------------------------------------
instance Eq (MetaKv a) where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show (MetaKv a) where
        show (MetaKv u _) = "$" ++ show u

instance Ord (MetaKv a) where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

instance Eq (Kind a) where
        StarK == StarK = True
        ArrK kn11 kn12 == ArrK kn21 kn22 = kn11 == kn12 && kn21 == kn22
        MetaK mk1 == MetaK mk2 = mk1 == mk2
        _ == _ = False

instance Show (Kind a) where
        show StarK = "StarK"
        show (ArrK kn1 kn2) = "ArrK " ++ show kn1 ++ " " ++ show kn2
        show (MetaK mk) = show mk

instance Substitutable (Kind a) where
        subst _ = id

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Kind a) where
        pretty StarK = "*"
        pretty (ArrK k1 k2) = pprkind k1 <+> pretty k2
        pretty (MetaK m) = viaShow m

pprkind :: Kind a -> Doc ann
pprkind StarK = "*"
pprkind kn = parens (pretty kn)
