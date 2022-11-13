{-# LANGUAGE OverloadedStrings #-}

module Plato.Parsing.Rename where

import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Common.Name.Reader
import Plato.Syntax.Parsing
import Plato.Types.Fixity

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Data.List (find)
import qualified Data.Map.Strict as M
import Plato.Common.Error
import Plato.Common.SrcLoc
import Prettyprinter

----------------------------------------------------------------
-- Renaming
----------------------------------------------------------------
class Rename f where
        rename :: MonadThrow m => f RdrName -> ReaderT GlbNameEnv m (f GlbName)

instance Rename Located where
        rename (L sp (Unqual n)) = asks $ \env -> L sp $ lookupGlbNameEnv env (L sp n)
        rename (L sp (Qual modn n)) = do
                env <- ask
                unless (n `M.member` env) $
                        throwLocErr sp $
                                hsep ["No module named", squotes $ pretty n, "is imported"]
                return $ L sp $ externalName modn (L sp n)

instance Rename Expr where
        rename (VarE var) = VarE <$> rename var
        rename (AppE fun arg) = AppE <$> rename `traverse` fun <*> rename `traverse` arg
        rename (OpE lhs op rhs) = OpE <$> rename `traverse` lhs <*> rename op <*> rename `traverse` rhs
        rename (LamE args body) = do
                argNamesCheck args
                LamE args <$> rename `traverse` body
        rename (LetE decs body) = LetE <$> mapM (rename `traverse`) decs <*> rename `traverse` body
        rename (CaseE match alts) = do
                match' <- rename `traverse` match
                alts' <- forM alts $ \(pat, body) -> do
                        pat' <- rename `traverse` pat
                        body' <- rename `traverse` body
                        return (pat', body')
                return $ CaseE match' alts'
        rename (FactorE exp) = FactorE <$> rename `traverse` exp

instance Rename Pat where
        rename (ConP con pats) = ConP <$> rename con <*> mapM (rename `traverse`) pats
        rename (VarP var) = return $ VarP var
        rename WildP = return WildP

instance Rename Type where
        rename (VarT var) = return $ VarT var
        rename (ConT con) = ConT <$> rename con
        rename (AppT funty argty) = AppT <$> rename `traverse` funty <*> rename `traverse` argty
        rename (ArrT argty resty) = ArrT <$> rename `traverse` argty <*> rename `traverse` resty
        rename (AllT args bodyty) = do
                argNamesCheck args
                AllT args <$> rename `traverse` bodyty

instance Rename Decl where
        rename (FuncD var args body) = do
                argNamesCheck args
                FuncD var args <$> rename `traverse` body
        rename (FuncTyD var body_ty) = FuncTyD var <$> rename `traverse` body_ty
        rename (FixityD fix prec op) = return $ FixityD fix prec op

instance Rename TopDecl where
        rename (DataD con args fields) = do
                argNamesCheck args
                fields' <- forM fields $ \(dcon, tys) -> do
                        tys' <- mapM (rename `traverse`) tys
                        return (dcon, tys')
                return $ DataD con args fields'
        rename (TypeD con args bodyty) = do
                argNamesCheck args
                bodyty' <- rename `traverse` bodyty
                return $ TypeD con args bodyty'
        rename (Decl d) = Decl <$> rename `traverse` d
        rename (Eval e) = Eval <$> rename `traverse` e

instance Rename Program where
        rename (Program mb_modn imp_modns topds) = do
                names <- forM topds $ \(L sp tds) -> case tds of
                        DataD con _ _ -> return con
                        TypeD con _ _ -> return con
                        Decl (L _ (FuncTyD var _)) -> return var
                        _ -> throwLocErr sp ""
                namesCheck names
                let modn = case mb_modn of
                        Just modn -> modn
                        Nothing -> L NoSpan mainModname
                topds' <-
                        local (\env -> foldr (insertGlbNameEnv $ unLoc modn) env names) $
                                mapM (rename `traverse`) topds
                return $ Program (Just modn) imp_modns topds'

renameFixityEnv :: GlbNameEnv -> FixityEnv (Located Name) -> FixityEnv GlbName
renameFixityEnv = M.mapKeys . lookupGlbNameEnv

----------------------------------------------------------------
-- namesCheck
----------------------------------------------------------------
namesCheck :: MonadThrow m => [LName] -> m ()
namesCheck ns = loop ns
    where
        loop :: MonadThrow m => [LName] -> m ()
        loop [] = return ()
        loop (L _ n1 : ns) = case find ((n1 ==) . unLoc) ns of
                Just (L sp2 n2) ->
                        throwLocErr sp2 $
                                hsep
                                        [ "Multiple declarations for"
                                        , squotes $ pretty n2
                                        ]
                Nothing -> loop ns

argNamesCheck :: MonadThrow m => [LArg] -> m ()
argNamesCheck ns = case [(n1, sp1, sp2) | L sp1 n1 <- ns, L sp2 n2 <- ns, n1 == n2, sp1 /= sp2] of
        [] -> return ()
        (n, sp1, sp2) : _ ->
                throwLocErr (combineSpans sp1 sp2) $
                        hsep
                                [ "Conflicting definitions for"
                                , squotes $ pretty n
                                ]
