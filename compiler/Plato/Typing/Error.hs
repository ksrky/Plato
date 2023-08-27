module Plato.Typing.Error (
        UnificationFail (..),
        InfiniteType (..),
        SubsCheckFail (..),
        instErrHandler,
        unifunErrHandler,
) where

import Control.Exception.Safe
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing

data UnificationFail = UnificationFail deriving (Show)
data InfiniteType = InfiniteType deriving (Show)
data SubsCheckFail = SubsCheckFail deriving (Show)

instance Exception UnificationFail
instance Exception InfiniteType
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
        , Handler $ \InfiniteType ->
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
