module Plato.Core.Context where

import Plato.Core.Subst
import Plato.Syntax.Core
import Plato.Types.Error
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Exception.Safe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V
import Prettyprinter

type Context a = V.Vector (a, Binding)

emptyContext :: Context a
emptyContext = V.empty

lookupContext :: Eq a => a -> Context a -> Maybe Binding
lookupContext k ctx = do
        ((x, y), tl) <- V.uncons ctx
        if k == x then Just y else lookupContext k tl

addBinding :: MonadThrow m => a -> Binding -> Context a -> m (Context a)
addBinding x bind ctx = return $ V.cons (x, bind) ctx

addName :: MonadThrow m => a -> Context a -> m (Context a)
addName x = addBinding x NameBind

addNameList :: MonadThrow m => [a] -> Context a -> m (Context a)
addNameList = flip $ foldM (flip addName)

addFreshName :: Name -> Context Name -> Context Name
addFreshName x ctx = case lookupContext x ctx of
        Just _ -> addFreshName (x{nameText = T.snoc (nameText x) '\''}) ctx
        Nothing -> V.cons (x, NameBind) ctx

pickFreshName :: Name -> Context Name -> (Name, Context Name)
pickFreshName x ctx = case lookupContext x ctx of
        Just _ -> pickFreshName (x{nameText = T.snoc (nameText x) '\''}) ctx
        Nothing -> (x, V.cons (x, NameBind) ctx)

index2name :: Context a -> Int -> a
index2name ctx x = fst (ctx V.! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt

getBinding :: Context a -> Int -> Binding
getBinding ctx i = bindingShift (i + 1) (snd $ ctx V.! i)

getType :: MonadThrow m => Context Name -> Int -> m Ty
getType ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ tyT -> return tyT
        _ -> throwUnexpErr $ "Wrong kind of binding for variable" <+> pretty (index2name ctx i)

getKind :: MonadThrow m => Context Name -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ knK -> return knK
        _ -> throwError $ hsep ["getkind: Wrong kind of binding for variable", pretty (index2name ctx i)]

getVarIndex :: MonadThrow m => Context GlbName -> GlbName -> m Int
getVarIndex ctx x = case V.elemIndex x (V.map fst ctx) of
        Just i -> return i
        Nothing -> throwLocErr (g_loc x) $ "Unbound variable name: '" <> pretty x <> "'"