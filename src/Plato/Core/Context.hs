module Plato.Core.Context where

import Control.Monad.State
import qualified Plato.Common.Name as N
import Plato.Core.Syntax

type Context = [(N.Name, Binding)]

emptyContext :: Context
emptyContext = []

addbinding :: N.Name -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

getbinding :: Context -> Int -> Binding
getbinding ctx i =
        if i > length ctx
                then bindingShift (i + 1) (snd $ ctx !! i)
                else error $ "Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        TyVarBind -> TyVarBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyAbbBind tyT -> TyAbbBind (typeShift d tyT)
        TmAbbBind t tyT_opt -> do
                let tyT_opt' = case tyT_opt of
                        Nothing -> Nothing
                        Just tyT -> Just $ typeShift d tyT
                TmAbbBind (termShift d t) tyT_opt'

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> tyT
        (_, TmAbbBind _ (Just tyT)) -> tyT
        (_, TmAbbBind _ Nothing) -> error $ "No type recorded for variable " ++ N.name2str (index2name ctx i)
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

pickfreshname :: Monad m => N.Name -> StateT Context m N.Name
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (N.appendstr x "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> N.Name
index2name ctx x = fst (ctx !! x)