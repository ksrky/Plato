module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Core.Subst

import Plato.Syntax.Core

import Control.Exception.Safe
import qualified Data.Vector as V
import Prettyprinter

emptyContext :: Context
emptyContext = V.empty

addBinding :: Name -> Binding -> Context -> Context
addBinding x bind = V.cons (x, bind)

addName :: Name -> Context -> Context
addName x = addBinding x NameBind

addNameList :: [Name] -> Context -> Context
addNameList = flip (foldl (flip addName))

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

getVarIndex :: MonadThrow m => Context -> Located Name -> m Int
getVarIndex ctx (L sp x) = case V.elemIndex x (V.map fst ctx) of
        Just i -> return i
        Nothing -> throwLocErr sp $ "Unbound variable name: '" <> pretty x <> "'"