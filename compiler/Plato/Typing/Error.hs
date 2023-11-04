module Plato.Typing.Error
    ( OccursCheckFail (..)
    , SubsCheckFail (..)
    , UnificationFail (..)
    , kcErrorHandler
    , tcErrorHandler
    , unifunErrorHandler
    ) where

import Control.Exception.Safe
import Control.Monad.IO.Class

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing
import Plato.Typing.Zonking

data UnificationFail = UnificationFail
    deriving (Show)
data OccursCheckFail = OccursCheckFail
    deriving (Show)
data SubsCheckFail = SubsCheckFail
    deriving (Show)

instance Exception UnificationFail
instance Exception OccursCheckFail
instance Exception SubsCheckFail

tcErrorHandler :: (MonadIO m, MonadCatch m) => Span -> Type -> Type -> [Handler m a]
tcErrorHandler sp ty_act ty_exp =
    [ Handler $ \UnificationFail -> do
        ty_act' <- zonk ty_act
        ty_exp' <- zonk ty_exp
        throwLocErr sp $ vsep
            [ "Couldn't unify expected type:" <+> pretty ty_exp'
            , "            with actual type:" <+> pretty ty_act']
    , Handler $ \OccursCheckFail -> do
        ty_act' <- zonk ty_act
        ty_exp' <- zonk ty_exp
        throwLocErr sp $ hsep
            [ "Infinite type:"
            , squotes $ pretty ty_exp'
            , "~"
            , squotes $ pretty ty_act']
    , Handler $ \SubsCheckFail -> do
        ty_act' <- zonk ty_act
        ty_exp' <- zonk ty_exp
        throwLocErr sp $ vsep
            [ "Couldn't instantiate actual type to expected type. (Impredicative types)"
            , "Expected type:" <+> pretty ty_exp'
            , "  Actual type:" <+> pretty ty_act']
    ]

unifunErrorHandler :: (MonadIO m, MonadCatch m) => Span -> Rho -> [Handler m a]
unifunErrorHandler sp rho =
    [ Handler $ \UnificationFail -> do
        rho' <- zonk rho
        throwLocErr sp $ vsep
            [ "Couldn't unify expected type: _ -> _"
            , "            with actual type:" <+> pretty rho']
    ]

kcErrorHandler :: (MonadIO m, MonadCatch m) => Span -> Kind -> Kind -> [Handler m a]
kcErrorHandler sp kn_exp kn_act =
    [ Handler $ \UnificationFail -> do
            kn_exp' <- zonk kn_exp
            kn_act' <- zonk kn_act
            throwLocErr sp $ vsep
                [ "Couldn't match expected kind with actual kind."
                , "Expected kind:" <+> pretty kn_exp'
                , "  Actual kind:" <+> pretty kn_act']
    , Handler $ \OccursCheckFail -> do
            kn_exp' <- zonk kn_exp
            kn_act' <- zonk kn_act
            throwLocErr sp $ hsep
                [ "Infinite kind:"
                , squotes $ pretty kn_exp', "~", squotes $ pretty kn_act']
    ]
