{-# LANGUAGE TupleSections #-}

module Plato.Typing.Renamer where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.State
import Data.List

type Names = [(Name, Name)]

data RenameState = RenameState
        { moduleName :: Maybe ModuleName
        , names :: Names
        , level :: Int
        }

initRenameState :: Maybe (Located ModuleName) -> RenameState
initRenameState modn =
        RenameState
                { moduleName = unLoc <$> modn
                , names = []
                , level = 0
                }

getWrapperName :: RenameState -> Name
getWrapperName st = case moduleName st of
        Just modn | level st == 0 -> mod2conName modn
        _ -> str2conName (':' : show (level st))

updateNames :: [FuncDecl] -> Name -> Names -> Names
updateNames decs name st = zip (map (\(FD x _ _) -> unLoc x) decs) (repeat name)

mkProj :: Located Name -> Located Name -> Expr
mkProj r = ProjE (noLoc $ VarE r)

mkValue :: Names -> Located Name -> Expr
mkValue names lx@(L sp x) = case lookup x names of
        Just r -> mkProj (noLoc r) lx
        Nothing -> VarE lx

-- | Renaming
class Rename a where
        rename :: MonadThrow m => RenameState -> a -> m a

instance Rename Expr where
        rename st (VarE x) = return $ mkValue (names st) x
        rename st (AbsE x ty e) = AbsE x ty <$> rename st `traverse` e
        rename st (AppE e1 e2) = AppE <$> rename st `traverse` e1 <*> rename st `traverse` e2
        rename st (TAbsE xs e) = TAbsE xs <$> rename st `traverse` e
        rename st (TAppE e tys) = TAppE <$> rename st `traverse` e <*> pure tys
        rename st (LetE [dec] body) = do
                dec' <- rename st{level = level st + 1} dec
                LetE [dec'] <$> rename st `traverse` body
        rename st (LetE decs body) = do
                (dec, st') <- renameFuncDecls st decs
                body' <- rename st `traverse` body
                return $ LetrecE dec body'
        rename st (CaseE e ty alts) = do
                e' <- rename st `traverse` e
                alts' <- forM alts $ \(pat, body) -> (pat,) <$> rename st `traverse` body
                return $ CaseE e' ty alts'
        rename st e = return e

instance Rename FuncDecl where
        rename st (FD var body ty) = FD var <$> rename st `traverse` body <*> pure ty

renameFuncDecls :: MonadThrow m => RenameState -> [FuncDecl] -> m (FuncDecl, RenameState)
renameFuncDecls st decs = do
        let r = getWrapperName st
            st' = st{names = updateNames decs r (names st)}
        fields <- forM decs $ \(FD var body ty) -> do
                body' <- rename st' `traverse` body
                return (var, body')
        let fieldtys = [(var, ty) | FD var _ ty <- decs]
        return (FD (noLoc r) (noLoc $ RecordE fields) (noLoc $ RecordT fieldtys), st')
