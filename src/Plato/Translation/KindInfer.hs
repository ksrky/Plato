module Plato.Translation.KindInfer where

import Plato.Common.Name
import Plato.Core.Context
import qualified Plato.Core.Syntax as C
import Plato.Typing.Syntax as I

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Vect

data Kn = KStar | KArr Kn Kn | KVar String deriving (Eq, Show)

data Store = Store {memo :: [(Name, String)], constr :: M.Map String Kn, number :: Int}

emptyStore :: Store
emptyStore = Store{memo = [], constr = M.empty, number = 0}

fresh :: State Store String
fresh = do
        n <- gets number
        let freshname = "X" ++ show n
        store <- get
        put $ store{constr = M.insert freshname KStar (constr store), number = n + 1}
        return freshname

addabsname :: Name -> String -> State Store ()
addabsname x str = do
        store <- get
        put $ store{memo = (x, str) : memo store}

updateKind :: String -> Kn -> State Store ()
updateKind x k = do
        store <- get
        put $ store{constr = M.insert x k (constr store)}

appKind :: Kn -> Kn -> State Store Kn
appKind k1 k2 = case k1 of
        KArr k11 k12 | k11 == k2 -> return k12
        KArr (KVar x) k12 -> updateKind x k2 >> return k12
        KVar x -> do
                n <- fresh
                updateKind x (KArr k2 (KVar n))
                return $ KVar n
        _ -> return KStar -- error

getKind :: Name -> State Store Kn
getKind x = do
        m <- gets memo
        a <- case lookup x m of
                Just a -> return a
                Nothing -> error $ "want " ++ show x ++ ", memo=" ++ show m
        c <- gets constr
        case M.lookup a c of
                Just KStar -> return $ KVar a
                Just k -> walk k
                Nothing -> error "getKind"
    where
        walk :: Kn -> State Store Kn
        walk (KArr k1 k2) = KArr <$> walk k1 <*> walk k2
        walk KStar = return KStar
        walk (KVar x) = do
                c <- gets constr
                case M.lookup x c of
                        Just k -> walk k
                        Nothing -> error "walk"

infer :: Type -> State Store Kn
infer ty = case ty of
        VarType fi x -> getKind x
        ArrType _ ty1 ty2 -> do
                infer ty1
                infer ty2
                return KStar
        AllType _ x ty -> do
                n <- fresh
                addabsname x n
                infer ty
        AbsType _ x ty -> do
                n <- fresh
                addabsname x n
                k2 <- infer ty
                k1 <- getKind x
                return $ KArr k1 k2
        AppType _ ty1 ty2 -> do
                k2 <- infer ty2
                k1 <- infer ty1
                appKind k1 k2
        RecType _ x ty -> do
                n <- fresh
                addabsname x n
                infer ty
        RecordType _ fields -> do
                mapM_ (\(_, ty) -> infer ty) fields
                return KStar
        SumType fields -> do
                forM_ fields $ \(_, _, tys) -> mapM infer tys
                return KStar

replaceStar :: Kn -> State Store C.Kind
replaceStar k = case k of
        KStar -> return C.KnStar
        KArr k1 k2 -> C.KnArr <$> replaceStar k1 <*> replaceStar k2
        KVar x -> do
                updateKind x KStar
                return C.KnStar

convertKind :: C.Kind -> Kn
convertKind C.KnStar = KStar
convertKind (C.KnArr knK1 knK2) = KArr (convertKind knK1) (convertKind knK2)

initStore :: Context -> State Store ()
initStore ctx = forM_ ctx $ \(x, bind) -> case bind of
        C.TyVarBind knK -> do
                n <- fresh
                addabsname x n
                updateKind n (convertKind knK)
        _ -> return ()

kindInfer :: Context -> Type -> C.Kind
kindInfer ctx ty =
        let store = initStore ctx `execState` emptyStore
            (k, store') = infer ty `runState` store
            (knK, store'') = replaceStar k `runState` store'
         in knK
