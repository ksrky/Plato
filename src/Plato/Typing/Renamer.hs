{-# LANGUAGE TupleSections #-}

module Plato.Typing.Renamer where

import Plato.Common.GenName
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.State
import Data.List

type Names = [(GenName, GenName)]

data RenameState = RenameState
        { moduleName :: Maybe ModuleName
        , names :: Names
        , level :: Int
        }

initRenameState :: Maybe ModuleName -> RenameState
initRenameState modn =
        RenameState
                { moduleName = modn
                , names = []
                , level = 0
                }

getWrapperName :: RenameState -> Name
getWrapperName st = case moduleName st of
        Just modn | level st == 0 -> mod2conName modn --tmp: conflict
        _ -> str2conName (':' : show (level st))

updateNames :: [FuncD] -> GenName -> Names -> Names
updateNames decs name st = zip (map (\(FuncD x _ _) -> x) decs) (repeat name)

mkProj :: GenName -> GenName -> Expr
mkProj r = ProjE (VarE r)

mkValue :: Names -> GenName -> Expr
mkValue names x = case lookup x names of
        Just r -> mkProj r x
        Nothing -> VarE x

-- | Renaming
class Rename a where
        rename :: MonadThrow m => RenameState -> a -> m a

instance Rename Expr where
        rename st (VarE x) = return $ mkValue (names st) x
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
        rename st e = return e

instance Rename FuncD where
        rename st (FuncD var body ty) = FuncD var <$> rename st body <*> pure ty

renameFuncDs :: MonadThrow m => RenameState -> [FuncD] -> m (FuncD, RenameState)
renameFuncDs st decs = do
        let r = newName $ getWrapperName st
            st' = st{names = updateNames decs r (names st)}
        fields <- forM decs $ \(FuncD var body ty) -> do
                body' <- rename st' body
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return (FuncD r (RecordE fields) (RecordT fieldtys), st')
