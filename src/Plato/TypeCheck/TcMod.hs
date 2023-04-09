module Plato.TypeCheck.TcMod where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Data.IORef
import qualified Data.Set as S
import Prettyprinter

import Plato.Common.Global
import Plato.Syntax.Typing.Module
import Plato.Types.Error
import Plato.Typing.Monad

tcMod :: (MonadReader env m, HasUnique env, MonadIO m, MonadThrow m) => Mod -> Typ m Mod
tcMod = undefined

tcBind :: (MonadReader env m, HasUnique env, MonadIO m, MonadThrow m) => Bind -> Typ m Bind
tcBind = undefined

matchSig :: MonadThrow m => Sig -> Sig -> Typ m ()
matchSig (SigDecls decs1) (SigDecls decs2) = undefined

matchDecl :: MonadThrow m => Decl -> Decl -> Typ m ()
matchDecl (ValueDecl id1 ty1) (ValueDecl id2 vty2) = undefined