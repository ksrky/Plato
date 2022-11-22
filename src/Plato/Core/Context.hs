module Plato.Core.Context where

import Plato.Core.Subst
import Plato.Syntax.Core
import Plato.Types.Error
import Plato.Types.Name.Global

import Control.Exception.Safe
import qualified Data.Vector as V
import Prettyprinter

type Context = V.Vector (GlbName, Binding)

emptyContext :: Context
emptyContext = V.empty

lookupContext :: GlbName -> Context -> Maybe Binding
lookupContext k ctx = do
        ((x, y), tl) <- V.uncons ctx
        if k == x then Just y else lookupContext k tl

addBinding :: GlbName -> Binding -> Context -> Context
addBinding x bind = V.cons (x, bind)

addName :: GlbName -> Context -> Context
addName x = addBinding x NameBind

addNameList :: [GlbName] -> Context -> Context
addNameList = flip (foldl (flip addName))

addSomething :: Context -> Context
addSomething = addBinding dummyGlbName NameBind

addSomeName :: Binding -> Context -> Context
addSomeName = addBinding dummyGlbName

index2name :: Context -> Int -> GlbName
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

getType :: MonadThrow m => Context -> Int -> m Ty
getType ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ tyT -> return tyT
        _ -> throwUnexpErr $ "Wrong kind of binding for variable" <+> pretty (index2name ctx i)

getKind :: MonadThrow m => Context -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ knK -> return knK
        _ -> throwError $ hsep ["getkind: Wrong kind of binding for variable", pretty (index2name ctx i)]

getVarIndex :: MonadThrow m => Context -> GlbName -> m Int
getVarIndex ctx x = case V.elemIndex x (V.map fst ctx) of
        Just i -> return i
        Nothing -> throwLocErr (g_loc x) $ "Unbound variable name: '" <> pretty x <> "'" <+> viaShow (g_loc x) <+> viaShow (g_sort x)