{-# LANGUAGE TupleSections #-}

module Plato.Typing.Renamer where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Text as T

type Names = [(GlbName, GlbName)]

data RenameState = RenameState
        { moduleName :: Maybe ModuleName
        , internalNames :: Names
        , externalNames :: Names
        , level :: Int
        }

names :: RenameState -> Names
names st = internalNames st ++ externalNames st

emptyRenameState :: RenameState
emptyRenameState =
        RenameState
                { moduleName = Nothing
                , internalNames = []
                , externalNames = []
                , level = 0
                }

getWrapperName :: RenameState -> GlbName
getWrapperName st = case moduleName st of
        Just modn | level st == 0 -> newName $ mod2conName modn
        _ -> newName $ str2conName (':' : show (level st))

mkNames :: [FuncD] -> GlbName -> Names
mkNames decs name = zip (map (\(FuncD x _ _) -> x) decs) (repeat name)

isExternal :: RenameState -> GlbName -> Bool
isExternal st n =
        nameSpace (g_name n) == ConName
                && T.head (nameText (g_name n)) /= ':'
                && (mod2conName <$> moduleName st) /= Just (g_name n)

mkValue :: RenameState -> GlbName -> Expr
mkValue st x = case lookup x (internalNames st) of
        Just r -> ProjE (VarE r) x
        Nothing -> case lookup x (externalNames st) of
                Just r -> ProjE (VarE r) x{g_sort = External}
                Nothing -> VarE x

-- | Renaming
class Rename a where
        rename :: MonadThrow m => RenameState -> a -> m a

instance Rename Expr where
        rename st (VarE x) = return $ mkValue st x
        rename st (AbsE x ty e) = AbsE x ty <$> rename st e
        rename st (AppE e1 e2) = AppE <$> rename st e1 <*> rename st e2
        rename st (TAbsE xs e) = TAbsE xs <$> rename st e
        rename st (TAppE e tys) = TAppE <$> rename st e <*> pure tys
        rename st (LetE decs body) = do
                (dec, st') <- renameFuncDs st decs
                body' <- rename st' body
                return $ LetE [dec] body'
        rename st (CaseE e ty alts) = do
                e' <- rename st e
                alts' <- forM alts $ \(pat, body) -> (pat,) <$> rename st body
                return $ CaseE e' ty alts'
        rename _ e = return e

instance Rename FuncD where
        rename st (FuncD var body ty) = FuncD var <$> rename st body <*> pure ty

renameFuncDs :: MonadThrow m => RenameState -> [FuncD] -> m (FuncD, RenameState)
renameFuncDs st decs = do
        let r = getWrapperName st
            st' = st{internalNames = mkNames decs r ++ internalNames st}
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- rename st' body
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return (FuncD r (RecordE fields) (RecordT fieldtys), st')
