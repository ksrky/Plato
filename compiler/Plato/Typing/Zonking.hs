{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Zonking (Zonking (..)) where

import Control.Monad.IO.Class

import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

class Zonking a where
        zonk :: (MonadIO m) => a -> m a

instance (Zonking a) => Zonking (Located a) where
        zonk = traverse zonk

instance (Zonking a, Zonking b) => Zonking (a, b) where
        zonk (a, b) = (,) <$> zonk a <*> zonk b

instance (Zonking a) => Zonking [a] where
        zonk = mapM zonk

instance (Zonking a) => Zonking (Block a) where
        zonk = mapM zonk

instance Zonking (Expr 'Typed) where
        zonk (VarE id) = return (VarE id)
        zonk (AppE fun arg) = AppE <$> zonk fun <*> zonk arg
        zonk (AbsE var ty body) = AbsE var <$> zonk ty <*> zonk body
        zonk (TAppE exp tys) = TAppE <$> zonk exp <*> mapM zonk tys
        zonk (TAbsE qnts body) = do
                qnts' <- mapM (\(tv, kn) -> (tv,) <$> zonk kn) qnts
                TAbsE qnts' <$> zonk body
        zonk (LetE bnds body) = do
                bnds' <- zonk bnds
                body' <- zonk body
                return $ LetE bnds' body'
        zonk (CaseE test ann_ty alts) = do
                test' <- zonk test
                ann_ty' <- zonk ann_ty
                alts' <- mapM (\(p, e) -> (,) <$> mapM zonk p <*> zonk e) alts
                return $ CaseE test' ann_ty' alts'

instance Zonking Pat where
        zonk (TagP con args) = TagP con <$> mapM (\(arg, ty) -> (arg,) <$> zonk ty) args
        zonk pat = return pat

instance Zonking TyVar where
        zonk = return

instance Zonking Type where
        zonk (VarT tv) = return (VarT tv)
        zonk (ConT tc) = return (ConT tc)
        zonk (ArrT arg res) = ArrT <$> zonk `traverse` arg <*> zonk `traverse` res
        zonk (AllT qnts ty) = AllT <$> zonk qnts <*> zonk `traverse` ty
        zonk (AppT fun arg) = AppT <$> zonk `traverse` fun <*> zonk `traverse` arg
        zonk (MetaT tv) =
                readMetaTv tv >>= \case
                        Nothing -> return (MetaT tv)
                        Just tau -> do
                                tau' <- zonk tau
                                writeMetaTv tv tau'
                                return tau'

instance Zonking Kind where
        zonk StarK = return StarK
        zonk (ArrK kn1 kn2) = do
                kn1' <- zonk kn1
                kn2' <- zonk kn2
                return (ArrK kn1' kn2')
        zonk (MetaK kv) =
                readMetaKv kv >>= \case
                        Nothing -> return (MetaK kv)
                        Just kn -> do
                                kn' <- zonk kn
                                writeMetaKv kv kn'
                                return kn'

instance Zonking (Bind 'Typed) where
        zonk (Bind (id, ty) exp) = Bind <$> ((id,) <$> zonk ty) <*> zonk exp

instance Zonking (TypDefn 'Typed) where
        zonk (DatDefn id kn params constrs) =
                DatDefn id
                        <$> zonk kn
                        <*> mapM (\(p, kn) -> (p,) <$> zonk kn) params
                        <*> mapM (\(con, ty) -> (con,) <$> zonk ty) constrs
