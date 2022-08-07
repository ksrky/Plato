module Plato.Core.Context where

import Plato.Common.Error
import qualified Plato.Common.Name as N
import Plato.Common.Vect
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State
import Plato.Common.Info

----------------------------------------------------------------
-- Core monad
----------------------------------------------------------------
type Core m = StateT Context m

evalCore :: Monad m => Core m a -> Core m a
evalCore f = do
        ctx <- get
        val <- f
        put ctx
        return val

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
type Context = Vect (N.Name, Binding)

emptyContext :: Context
emptyContext = empty

initContext :: Context --tmp
initContext = cons (N.str2tyConName "String", TyVarBind KnStar) $ cons (N.str2tyConName "Float", TyVarBind KnStar) emptyContext

addbinding :: MonadThrow m => N.Name -> Binding -> Core m ()
addbinding x bind = modify $ \ctx -> cons (x, bind) ctx

addbinding_ :: N.Name -> Binding -> Context -> Context
addbinding_ x bind = cons (x, bind)

{-pickfreshname :: Monad m => N.Name -> Binding -> Core m N.Name
pickfreshname x bind = state $ \ctx -> case look x ctx of
        Just _ -> pickfreshname (N.appendstr x "'") bind `runState` ctx
        Nothing -> (x, cons (x, bind) ctx)
-}
isuniquename :: MonadThrow m => Info -> N.Name -> Binding -> Core m ()
isuniquename fi x bind = do
        ctx <- get
        case look x ctx of
                Just _ -> throwError fi $ N.name2str x ++ " is already used."
                Nothing -> addbinding x bind

-- from index
getbinding :: Context -> Int -> Binding
getbinding ctx i = bindingShift (i + 1) (snd $ ctx ! i)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case getbinding ctx i of
        VarBind tyT -> tyT
        TmAbbBind _ (Just tyT) -> tyT
        TmAbbBind _ Nothing -> unreachable $ "No type recorded for variable " ++ N.name2str (index2name ctx i)
        _ -> unreachable $ "getTypeFromContext: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

getTyAbb :: MonadThrow m => Info -> Int -> Core m Ty
getTyAbb fi i = do
        ctx <- get
        case getbinding ctx i of
                TyAbbBind tyT _ -> return tyT
                b -> throwError fi $ "Wrong kind of binding for type: " ++ N.name2str (index2name ctx i)

getVarBind :: MonadThrow m => Info -> Int -> Core m Ty
getVarBind fi i = do
        ctx <- get
        case getbinding ctx i of
                VarBind tyT -> return tyT
                _ -> throwError fi $ "Wrong kind of binding for variable: " ++ N.name2str (index2name ctx i)

getTmAbb :: MonadThrow m => Info -> Int -> Core m Ty
getTmAbb fi i = do
        ctx <- get
        case getbinding ctx i of
                TmAbbBind _ (Just tyT) -> return tyT
                b -> throwError fi $ "Wrong kind of binding for variable: " ++ show b ++ "\n" ++ N.name2str (index2name ctx i)

-- from Name
getTyAbbFromName :: MonadThrow m => Info -> N.Name -> Core m Ty
getTyAbbFromName fi n = do
        ctx <- get
        i <- name2index fi ctx n
        getTyAbb fi i

getVarBindFromName :: MonadThrow m => Info -> N.Name -> Core m Ty
getVarBindFromName fi n = do
        ctx <- get
        i <- name2index fi ctx n
        getVarBind fi i

getTmAbbFromName :: MonadThrow m => Info -> N.Name -> Core m Ty
getTmAbbFromName fi n = do
        ctx <- get
        i <- name2index fi ctx n
        getTmAbb fi i

index2name :: Context -> Int -> N.Name
index2name ctx i = fst (ctx ! i)

name2index :: MonadThrow m => Info -> Context -> N.Name -> m Int
name2index fi ctx x = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> throwError fi $ "Unbound variable name: " ++ N.name2str x
