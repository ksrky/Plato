{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Env (
        Binding (..),
        TypEnv,
        EnvManager (..),
        extendSpec,
        extendSpecs,
) where

import Control.Exception.Safe
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing

data Binding
        = ValBinding Type
        | TypBinding Kind
        deriving (Eq, Show)

type TypEnv = IdentMap Binding

class EnvManager a where
        extend :: Ident -> a -> TypEnv -> TypEnv
        extendList :: [(Ident, a)] -> TypEnv -> TypEnv
        find :: MonadThrow m => Ident -> TypEnv -> m a
        extendList l env = foldr (uncurry extend) env l

instance EnvManager a => EnvManager (Located a) where
        extend id x = extend id (unLoc x)
        find = ((noLoc <$>) .) . find

instance EnvManager Type where
        extend id ty = M.insert id (ValBinding ty)
        find id env =
                lookupIdent id env >>= \case
                        ValBinding ty -> return ty
                        _ ->
                                throwLocErr (getLoc id) $
                                        hsep [squotes $ pretty id, "is not a term-level identifier"]

instance EnvManager Kind where
        extend id kn = M.insert id (TypBinding kn)
        find id env =
                lookupIdent id env >>= \case
                        TypBinding kn -> return kn
                        _ ->
                                throwLocErr (getLoc id) $
                                        hsep [squotes $ pretty id, "is not a type-level identifier"]

extendSpec :: Spec -> TypEnv -> TypEnv
extendSpec (ValSpec id ty) = extend id ty
extendSpec (TypSpec id kn) = extend id kn

extendSpecs :: [Spec] -> TypEnv -> TypEnv
extendSpecs = flip $ foldr extendSpec