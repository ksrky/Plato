module Plato.Typing.Env where

import Plato.Common.Name
import qualified Plato.Common.Table as T
import qualified Plato.Common.Vect as V
import Plato.Syntax.Typing
import Plato.Typing.Error

import Control.Monad.State
import Plato.Common.SrcLoc

data Binding = ExprBind Type | TypeBind Kind

data TypState = TypState
        { context :: V.Vect (Name, Binding)
        , typeSubst :: V.Vect (Type, Type)
        , kindTable :: T.Table Kind
        , typeCounter :: Int
        , kindCounter :: Int
        }

emptyTypState :: TypState
emptyTypState =
        TypState
                { context = V.empty
                , typeSubst = V.empty
                , kindTable = T.empty
                , typeCounter = 0
                , kindCounter = 0
                }

getTypeFromContext :: Name -> StateT TypState TypThrow Type
getTypeFromContext x = do
        ctx <- gets context
        case V.look x ctx of
                Just (ExprBind ty) -> return ty
                Just (TypeBind _) -> error ""
                Nothing -> error ""

getKind :: Name -> StateT TypState TypThrow Kind
getKind x = do
        st <- get
        case T.look (kindTable st) x of
                Just kn -> return kn
                Nothing -> return StarKind -- error

freshKindName :: Name -> StateT TypState TypThrow Name
freshKindName tyX = do
        cnt <- gets kindCounter
        let freshname = str2tyvarName $ "?k" ++ show cnt
        modify $ \st -> st{kindTable = T.enter tyX (VarKind freshname) (kindTable st), kindCounter = cnt + 1}
        return freshname

freshTyvar :: StateT TypState TypThrow Type
freshTyvar = do
        cnt <- gets kindCounter
        let freshname = str2tyvarName $ "?k" ++ show cnt
        return $ VarType $ noLoc freshname

getConstr :: Type -> StateT TypState TypThrow Type
getConstr ty1 = do
        subst <- gets typeSubst
        case V.look ty1 subst of
                Just ty2 -> return ty2
                Nothing -> lift $ throwTypError NoSpan ""

-- bind :: Type -> Type -> StateT TypState TypThrow ()
-- bind ty1 ty2 = modify $ \st -> st{typeSubst = V.cons (ty1, ty2) (typeSubst st)}