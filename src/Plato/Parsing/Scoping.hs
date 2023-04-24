{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Plato.Parsing.Scoping (scoping) where

import Control.Exception.Safe
import Control.Monad.Reader.Class
import qualified Data.Map.Strict as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Path
import Plato.Syntax.Parsing

type Scope = M.Map Name Ident

class HasScope a where
        getScope :: a -> Scope

instance HasScope Scope where
        getScope = id

class Scoping a where
        scoping :: (MonadReader env m, HasScope env, MonadThrow m) => a -> m a

instance Scoping a => Scoping (Located a) where
        scoping (L sp x) = L sp <$> scoping x

instance Scoping Ident where
        scoping id = do
                sc <- asks getScope
                case M.lookup (nameIdent id) sc of
                        Just id' -> return id'
                        Nothing ->
                                throwLocErr (getLoc id) $
                                        hsep ["Not in scope ", squotes $ pretty id]

instance Scoping Path where
        scoping (PIdent id) = do
                id' <- scoping id
                return $ PIdent id'
        scoping (PDot root field) = do
                root' <- scoping root
                return $ PDot root' field

instance Scoping Expr where
        scoping (VarE path) = VarE <$> scoping path
        scoping (AppE fun arg) = AppE <$> scoping fun <*> scoping arg
        scoping (OpE lhs op rhs) = OpE <$> scoping lhs <*> scoping op <*> scoping rhs
        scoping (LamE alts) =
                LamE
                        <$> mapM (\(pats, body) -> (pats,) <$> scoping body) alts
        scoping (LetE decs body) = LetE <$> mapM scoping decs <*> scoping body
        scoping (CaseE match alts) =
                CaseE <$> scoping match
                        <*> mapM (\(pat, body) -> (pat,) <$> scoping body) alts
        scoping (FactorE exp) = FactorE <$> scoping exp

instance Scoping Pat where
        scoping (ConP con pats) = ConP <$> scoping con <*> mapM scoping pats
        scoping (VarP var) = VarP <$> scoping var
        scoping WildP = return WildP

instance Scoping Type where
        scoping (VarT var) = VarT <$> scoping var
        scoping (ConT con) = ConT <$> scoping con
        scoping (AppT fun arg) = AppT <$> scoping fun <*> scoping arg
        scoping (ArrT arg res) = ArrT <$> scoping arg <*> scoping res
        scoping (AllT qnts body) = AllT qnts <$> scoping body

instance Scoping Decl where
        scoping (OpenD path) = OpenD <$> scoping path
        scoping (FixityD fix id) = FixityD fix <$> scoping id
        scoping (ModuleD id mod) = ModuleD id <$> scoping mod
        scoping (DataD id params fields) = DataD id params <$> mapM (\(con, body) -> (con,) <$> scoping body) fields
        scoping (FuncSigD id ty) = FuncSigD id <$> scoping ty
        scoping (FuncD id params exp) = FuncD <$> scoping id <*> mapM scoping params <*> scoping exp

instance Scoping Module where
        scoping (Module decs) = Module <$> mapM scoping decs