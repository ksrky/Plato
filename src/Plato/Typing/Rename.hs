{-# LANGUAGE TupleSections #-}

module Plato.Typing.Rename where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type NameTable = M.Map GlbName GlbName

data RenameState = RenameState
        { moduleName :: Maybe ModuleName
        , internalNameTable :: NameTable
        , externalNameTable :: NameTable
        , level :: Int
        }

names :: RenameState -> NameTable
names st = internalNameTable st `M.union` externalNameTable st

emptyRenameState :: RenameState
emptyRenameState =
        RenameState
                { moduleName = Nothing
                , internalNameTable = M.empty
                , externalNameTable = M.empty
                , level = 0
                }

getWrapperName :: RenameState -> GlbName
getWrapperName st = case moduleName st of
        Just modn | level st == 0 -> newName $ mod2conName modn
        _ -> newName $ str2conName (":l" ++ show (level st))

mkNameTable :: [FuncD] -> GlbName -> NameTable
mkNameTable decs name = M.fromList $ zip (map (\(FuncD x _ _) -> x) decs) (repeat name)

isExternal :: RenameState -> GlbName -> Bool
isExternal st n =
        nameSpace (g_name n) == ConName
                && T.head (nameText (g_name n)) /= ':'
                && (mod2conName <$> moduleName st) /= Just (g_name n)

mkValue :: RenameState -> GlbName -> Expr
mkValue st x = case M.lookup x (internalNameTable st) of
        Just r -> ProjE (VarE r) x
        Nothing -> case M.lookup x (externalNameTable st) of
                Just r -> ProjE (VarE r) x{g_sort = External}
                Nothing -> VarE x

-- | Renaming
rename :: MonadThrow m => RenameState -> Expr -> m Expr
rename st (VarE x) = return $ mkValue st x
rename st (AbsE x ty e) = AbsE x ty <$> rename st e
rename st (AppE e1 e2) = AppE <$> rename st e1 <*> rename st e2
rename st (TAbsE xs e) = TAbsE xs <$> rename st e
rename st (TAppE e tys) = TAppE <$> rename st e <*> pure tys
rename st (LetE decs body) = do
        (dec, st') <- renameFuncDs st{level = level st + 1} decs
        body' <- rename st' body
        return $ LetE [dec] body'
rename st (CaseE e ty alts) = do
        e' <- rename st e
        alts' <- forM alts $ \(pat, body) -> (pat,) <$> rename st body
        return $ CaseE e' ty alts'
rename _ e = return e

renameFuncDs :: MonadThrow m => RenameState -> [FuncD] -> m (FuncD, RenameState)
renameFuncDs st decs = do
        let r = getWrapperName st
            st' = st{internalNameTable = mkNameTable decs r `M.union` internalNameTable st}
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- rename st' body
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return (FuncD r (RecordE fields) (RecordT fieldtys), st')
