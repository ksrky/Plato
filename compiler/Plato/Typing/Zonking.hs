{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Zonking (
        Zonking (..),
) where

import Control.Monad.IO.Class

import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Typing.Utils

class Zonking a where
        zonk :: MonadIO m => a -> m a

instance Zonking a => Zonking (Located a) where
        zonk = traverse zonk

instance Zonking a => Zonking [a] where
        zonk = mapM zonk

instance Zonking (Expr 'Typed) where
        zonk (VarE id) = return (VarE id)
        zonk (AppE fun arg) = AppE <$> zonk fun <*> zonk arg
        zonk (AbsEok var ty body) = AbsEok var <$> zonk ty <*> zonk body
        zonk (TAppE exp tys) = TAppE <$> zonk exp <*> mapM zonk tys
        zonk (TAbsE qnts body) = do
                qnts' <- mapM (\(tv, kn) -> (tv,) <$> zonk kn) qnts
                TAbsE qnts' <$> zonk body
        zonk (LetEok bnds spcs body) = do
                bnds' <- mapM (\(id, exp) -> (id,) <$> zonk exp) bnds
                spcs' <- mapM (\(id, ty) -> (id,) <$> zonk ty) spcs
                body' <- zonk body
                return $ LetEok bnds' spcs' body'
        zonk (CaseEok match ann alts) = do
                match' <- zonk match
                ann' <- zonk ann
                alts' <- mapM (\(pats, exp) -> (pats,) <$> zonk exp) alts
                return $ CaseEok match' ann' alts'

instance Zonking Type where
        zonk (VarT tv) = return (VarT tv)
        zonk (ConT tc) = return (ConT tc)
        zonk (ArrT arg res) = ArrT <$> zonk `traverse` arg <*> zonk `traverse` res
        zonk (AllT qnts ty) = AllT <$> zonk qnts <*> zonk `traverse` ty
        zonk (AppT fun arg) = do
                AppT <$> zonk `traverse` fun <*> zonk `traverse` arg
        zonk (MetaT tv) = do
                mb_ty <- readMetaTv tv
                case mb_ty of
                        Nothing -> return (MetaT tv)
                        Just ty -> do
                                ty' <- zonk ty
                                writeMetaTv tv ty'
                                return ty'

instance Zonking Quant where
        zonk (tv, kn) = (tv,) <$> zonk kn

instance Zonking Kind where
        zonk StarK = return StarK
        zonk (ArrK kn1 kn2) = do
                kn1' <- zonk kn1
                kn2' <- zonk kn2
                return (ArrK kn1' kn2')
        zonk (MetaK kv) = do
                mb_kn <- readMetaKv kv
                case mb_kn of
                        Nothing -> return (MetaK kv)
                        Just kn -> do
                                kn' <- zonk kn
                                writeMetaKv kv kn'
                                return kn'

instance Zonking (Defn 'Typed) where
        zonk (FunDefnok id exp) = FunDefnok id <$> zonk exp
        zonk (TypDefn id ty) = TypDefn id <$> zonk ty
        zonk (DatDefnok id sig params constrs) =
                DatDefnok id
                        <$> zonk sig
                        <*> mapM (\(p, kn) -> (p,) <$> zonk kn) params
                        <*> mapM (\(con, ty) -> (con,) <$> zonk ty) constrs

instance Zonking Spec where
        zonk (ValSpec id ty) = ValSpec id <$> zonk ty
        zonk (TypSpec id kn) = TypSpec id <$> zonk kn

instance Zonking (Decl 'Typed) where
        zonk (DefnDecl def) = DefnDecl <$> zonk def
        zonk (SpecDecl spc) = SpecDecl <$> zonk spc