module Plato.Core.Context where

import qualified Plato.Common.Name as N
import Plato.Core.Syntax

import Control.Monad.State

type Context = [(N.Name, Binding)]

emptyContext :: Context
emptyContext = []

initContext :: Context
initContext = (N.str2name "String", TyVarBind KnStar) : (N.str2name "Float", TyVarBind KnStar) : emptyContext

addbinding :: N.Name -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

getbinding :: Context -> Int -> Int -> Binding
getbinding ctx i n =
        if i < n
                then bindingShift (i + 1) (snd $ ctx !! i)
                else error $ "Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx)

getbindingFromName :: N.Name -> State Context Binding
getbindingFromName n = do
        ctx <- get
        case lookup n ctx of
                Just bind -> return bind
                Nothing -> error $ "Variable lookup failure: " ++ show n

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
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

pickfreshname :: Monad m => N.Name -> Binding -> StateT Context m N.Name
pickfreshname x bind = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (N.appendstr x "'") bind `runState` ctx
        Nothing -> (x, (x, bind) : ctx)

isuniquename :: Monad m => N.Name -> Binding -> StateT Context m ()
isuniquename x bind = state $ \ctx -> case lookup x ctx of
        Just _ -> error $ N.name2str x ++ " is already used."
        Nothing -> ((), (x, bind) : ctx)

index2name :: Context -> Int -> N.Name
index2name ctx x = fst (ctx !! x)
