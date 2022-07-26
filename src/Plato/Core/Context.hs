module Plato.Core.Context where

import Plato.Common.Error
import qualified Plato.Common.Name as N
import Plato.Common.Vect
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State

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

getbinding :: Context -> Int -> Int -> Binding
getbinding ctx i n =
        if i < n
                then bindingShift (i + 1) (snd $ ctx ! i)
                else unreachable $ "Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx)

getbindingFromName :: MonadThrow m => N.Name -> Core m Binding
getbindingFromName n = do
        ctx <- get
        case look n ctx of
                Just bind -> return bind
                Nothing -> throwString $ "Variable lookup failure: " ++ show n

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)

getTypeFromContext :: Context -> Int -> Int -> Ty
getTypeFromContext ctx i n = case getbinding ctx i n of
        VarBind tyT -> tyT
        TmAbbBind _ (Just tyT) -> tyT
        TmAbbBind _ Nothing -> error $ "No type recorded for variable " ++ N.name2str (index2name ctx i)
        _ -> unreachable $ "getTypeFromContext: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

pickfreshname :: Monad m => N.Name -> Binding -> StateT Context m N.Name
pickfreshname x bind = state $ \ctx -> case look x ctx of
        Just _ -> pickfreshname (N.appendstr x "'") bind `runState` ctx
        Nothing -> (x, cons (x, bind) ctx)

isuniquename :: MonadThrow m => N.Name -> Binding -> Core m ()
isuniquename x bind = do
        ctx <- get
        case look x ctx of
                Just _ -> throwString $ N.name2str x ++ " is already used."
                Nothing -> addbinding x bind

index2name :: Context -> Int -> N.Name
index2name ctx x = fst (ctx ! x)

getVarIndex :: MonadThrow m => N.Name -> Context -> m Int
getVarIndex x ctx = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> throwString $ "Unbound variable name: " ++ N.name2str x
