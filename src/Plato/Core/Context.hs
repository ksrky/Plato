module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Global
import Plato.Common.Location
import Plato.Common.Name
import Plato.Core.Subst
import Plato.Syntax.Core

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Vector qualified as V
import Plato.Common.Ident
import Prettyprinter

type Context = V.Vector (Unique, Binding)

emptyContext :: Context
emptyContext = V.empty

lookupContext :: Name -> Context -> Maybe Binding
lookupContext x ctx = case V.uncons ctx of
        Just ((k, b), rest)
                | x == k -> Just b
                | otherwise -> lookupContext x rest
        Nothing -> Nothing

addBinding :: Ident -> Binding -> Context -> Context
addBinding id bind = V.cons (stamp id, bind)

addName :: Ident -> Context -> Context
addName id = addBinding id NameBind

addNameList :: [Name] -> Context -> Context
addNameList = flip (foldr addName)

index2name :: Context -> Int -> Name
index2name ctx x = fst (ctx V.! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt

getBinding :: Context -> Int -> Binding
getBinding ctx i = bindingShift (i + 1) (snd $ ctx V.! i)

getType :: MonadThrow m => Context -> Int -> m Type
getType ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ tyT -> return tyT
        _ -> throwUnexpErr $ "Wrong kind of binding for variable" <+> pretty (index2name ctx i)

getKind :: MonadThrow m => Context -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ knK -> return knK
        _ -> throwError $ hsep ["getkind: Wrong kind of binding for variable", pretty (index2name ctx i)]

getVarIndex :: Ident -> Reader Context Int
getVarIndex id = do
        ctx <- ask
        case V.elemIndex (stamp id) (V.map fst ctx) of
                Just i -> return i
                Nothing -> unreachable "Unbound variable name"
