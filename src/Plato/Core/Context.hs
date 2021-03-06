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

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
--type Context = [(N.Name, Binding)]

type Context = Vect (N.Name, Binding)

emptyContext :: Context
emptyContext = empty

initContext :: Context
initContext = cons (N.str2name "String", TyVarBind KnStar) $ cons (N.str2name "Float", TyVarBind KnStar) emptyContext

addbinding :: MonadThrow m => N.Name -> Binding -> Core m ()
addbinding x bind = modify $ \ctx -> cons (x, bind) ctx

addbinding_ :: N.Name -> Binding -> Context -> Context
addbinding_ x bind = cons (x, bind)

getbinding :: Context -> Int -> Binding
getbinding ctx i = bindingShift (i + 1) (snd $ ctx ! i)

getTyAbb :: MonadThrow m => Info -> N.Name -> Core m Ty
getTyAbb fi n = do
        ctx <- get
        case look n ctx of
                Just (TyAbbBind tyT _) -> return tyT
                _ -> throwError fi $ "Wrong kind of binding for type: " ++ N.name2str n
getVarBind :: MonadThrow m => Info -> N.Name -> Core m Ty
getVarBind fi n = do
        ctx <- get
        case look n ctx of
                Just (VarBind tyT) -> return tyT
                _ -> throwError fi $ "Wrong kind of binding for variable: " ++ N.name2str n

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

pickfreshname :: Monad m => N.Name -> Binding -> StateT Context m N.Name
pickfreshname x bind = state $ \ctx -> case look x ctx of
        Just _ -> pickfreshname (N.appendstr x "'") bind `runState` ctx
        Nothing -> (x, cons (x, bind) ctx)

isuniquename :: MonadThrow m => Info -> N.Name -> Binding -> Core m ()
isuniquename fi x bind = do
        ctx <- get
        case look x ctx of
                Just _ -> throwError fi $ N.name2str x ++ " is already used."
                Nothing -> addbinding x bind

index2name :: Context -> Int -> N.Name
index2name ctx i = fst (ctx ! i)

name2index :: MonadThrow m => Info -> Context -> N.Name -> m Int
name2index fi ctx x = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> throwError fi $ "Unbound variable name: " ++ N.name2str x
