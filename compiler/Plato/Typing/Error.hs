module Plato.Typing.Error (
        UnificationError (..),
        InfiniteTypeError (..),
        instErrHandler,
        unifunErrHandler,
) where

import Control.Exception.Safe
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing

data UnificationError = UnificationError deriving (Show)
data InfiniteTypeError = InfiniteTypeError deriving (Show)

instance Exception UnificationError
instance Exception InfiniteTypeError

instErrHandler :: MonadCatch m => Span -> Type -> Type -> [Handler m a]
instErrHandler sp ty_exp ty_sup =
        [ Handler $ \UnificationError ->
                throwLocErr sp $
                        vsep
                                [ "Infered type doesn't match expected type from the signature."
                                , "Expected type:" <+> pretty ty_sup
                                , " Infered type:" <+> pretty ty_exp
                                ]
        , Handler $ \InfiniteTypeError ->
                throwLocErr sp $
                        hsep
                                [ "Infinite type:"
                                , squotes $ pretty ty_exp
                                , "~"
                                , squotes $ pretty ty_sup
                                ]
        ]

unifunErrHandler :: MonadCatch m => Span -> Rho -> [Handler m a]
unifunErrHandler sp rho =
        [ Handler $ \(UnificationError{}) ->
                throwLocErr sp $
                        vsep
                                [ "Infered type doesn't match expected type from the signature."
                                , "Expected type: " <+> pretty rho
                                , "Infered function type"
                                ]
        ]
