{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.Rename where

import Plato.Common.Error
import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prettyprinter

type Names = S.Set GlbName

type NameTable = M.Map ModuleName Names

data RenameState = RenameState
        { moduleName :: GlbName
        , internalNames :: Names
        , externalNameTable :: NameTable
        , level :: Int
        }

--names :: RenameState -> NameTable
--names st = internalNameTable st `M.union` externalNameTable st

newRenameState :: Maybe (Located ModuleName) -> NameTable -> RenameState
newRenameState mbmodn nametab =
        RenameState
                { moduleName = case mbmodn of
                        Just (L sp modn) -> GlbName{g_sort = System, g_name = mod2conName modn, g_loc = sp}
                        Nothing -> newName $ str2conName "Main"
                , internalNames = S.empty
                , externalNameTable = nametab
                , level = 0
                }

getWrapperName :: RenameState -> GlbName
getWrapperName st =
        if level st == 0
                then moduleName st
                else newName $ str2conName (":l" ++ show (level st))

mkValue :: MonadThrow m => RenameState -> GlbName -> m Expr
mkValue st x =
        if x `S.member` internalNames st
                then return $ ProjE (VarE $ moduleName st) x
                else case M.keys $ M.filter (x `S.member`) (externalNameTable st) of
                        [r] -> return $ ProjE (VarE $ newName $ mod2conName r) x{g_sort = External}
                        [] -> return $ VarE x
                        _ -> throwLocErr (g_loc x) $ hsep ["Ambigous name:", pretty x]

-- | Renaming
rename :: MonadThrow m => RenameState -> Expr -> m Expr
rename st (VarE x) = mkValue st x
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
            st' = st{internalNames = S.fromList [x | (FuncD x _ _) <- decs] `S.union` internalNames st}
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- rename st' body
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return (FuncD r (RecordE fields) (RecordT fieldtys), st')
