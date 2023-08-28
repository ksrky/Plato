module Plato.Typing.Error (
        UnificationFail (..),
        OccursCheckFail (..),
        SubsCheckFail (..),
        instErrHandler,
        unifunErrHandler,
        kcErrorHandler,
) where

import Control.Exception.Safe
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing

data UnificationFail = UnificationFail deriving (Show)
data OccursCheckFail = OccursCheckFail deriving (Show)
data SubsCheckFail = SubsCheckFail deriving (Show)

instance Exception UnificationFail
instance Exception OccursCheckFail
instance Exception SubsCheckFail

instErrHandler :: MonadCatch m => Span -> Type -> Type -> [Handler m a]
instErrHandler sp ty_exp ty_sup =
        [ Handler $ \UnificationFail ->
                throwLocErr sp $
                        vsep
                                [ "Infered type doesn't match expected type from the signature."
                                , "Expected type:" <+> pretty ty_sup
                                , " Infered type:" <+> pretty ty_exp
                                ]
        , Handler $ \OccursCheckFail ->
                throwLocErr sp $
                        hsep
                                [ "Infinite type:"
                                , squotes $ pretty ty_exp
                                , "~"
                                , squotes $ pretty ty_sup
                                ]
        , Handler $ \SubsCheckFail ->
                throwLocErr sp $ hsep ["Subsumption check failed: ", pretty ty_exp <> comma, pretty ty_sup]
        ]

unifunErrHandler :: MonadCatch m => Span -> Rho -> [Handler m a]
unifunErrHandler sp rho =
        [ Handler $ \(UnificationFail{}) ->
                throwLocErr sp $
                        vsep
                                [ "Infered type doesn't match expected type from the signature."
                                , "Expected type: " <+> pretty rho
                                , "Infered function type"
                                ]
        ]

kcErrorHandler :: MonadCatch m => Span -> Kind -> Kind -> [Handler m a]
kcErrorHandler sp kn_exp kn_act =
        [ Handler $ \UnificationFail ->
                throwLocErr sp $
                        vsep
                                [ "Couldn't match expected kind with actual kind."
                                , "Expected kind:" <+> pretty kn_exp
                                , "  Actual kind:" <+> pretty kn_act
                                ]
        , Handler $ \OccursCheckFail ->
                throwLocErr sp $
                        hsep
                                [ "Infinite kind:"
                                , squotes $ pretty kn_exp
                                , "~"
                                , squotes $ pretty kn_act
                                ]
        ]