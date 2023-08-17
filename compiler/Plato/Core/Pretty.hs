module Plato.Core.Pretty where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Ident
import Plato.Common.Pretty
import Plato.Common.Uniq
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Normalise
import Plato.Core.Result
import Plato.Syntax.Core

class Print a where
        evalPrint :: (MonadReader e m, CoreEnv e, HasUniq e, MonadThrow m, MonadIO m) => a -> m (Doc ann)

instance Print Ident where
        evalPrint = return . prettyId

instance Print Val where
        -- original impl uses 'quote', but need to expand inside fold for the final result
        evalPrint a = evalPrint =<< nf [] a

instance Print (Clos Term) where
        evalPrint a = evalPrint =<< quote [] a

instance Print Ne where
        evalPrint a = evalPrint =<< quote [] a

instance Print Term where
        evalPrint = return . pretty