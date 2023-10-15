{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Linearize where

import Control.Monad.Writer
import Data.Graph

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper (splitConstrTy)

class Linearize a where
        linearize :: a -> Writer [Ident] a

instance Linearize a => Linearize (Located a) where
        linearize = traverse linearize

instance Linearize a => Linearize [a] where
        linearize = mapM linearize

instance Linearize (Expr 'Untyped) where
        linearize (VarE id) = do
                tell [id]
                return $ VarE id
        linearize (AppE fun arg) = AppE <$> linearize fun <*> linearize arg
        linearize (AbsE id mbty exp) = AbsE id mbty <$> linearize exp
        linearize (LetE binds exp) = do
                bindss <- linBinds binds
                exp' <- linearize exp
                return $ unLoc $ foldr (\b e -> sL b e $ LetE b e) exp' bindss
        linearize (CaseE exp alts) = do
                exp' <- linearize exp
                alts' <- mapM (\(p, e) -> (p,) <$> linearize e) alts
                return $ CaseE exp' alts'
        linearize (ClauseE cls) = ClauseE <$> mapM (\(ps, e) -> (ps,) <$> linearize e) cls

instance Linearize Type where
        linearize (VarT tv) = return $ VarT tv
        linearize (ConT tc) = do
                tell [tc]
                return $ ConT tc
        linearize (ArrT arg_ty res_ty) = ArrT <$> linearize arg_ty <*> linearize res_ty
        linearize (AllT qns body) = AllT qns <$> linearize body
        linearize (AppT fun_ty arg_ty) = AppT <$> linearize fun_ty <*> linearize arg_ty
        linearize (MetaT tv) = return $ MetaT tv

instance Linearize (Bind 'Untyped) where
        linearize (Bind idty exp) = Bind idty <$> mapM linearize exp

linBinds :: Block (XBind 'Untyped) -> Writer [Ident] [Block (XBind 'Untyped)]
linBinds (Mutrec bnds) = do
        graph <- forM bnds $ \bnd@(L _ (Bind (par, _) _)) -> do
                (bnd', chs) <- listen $ linearize bnd
                return (bnd', stamp par, map stamp chs)
        return $ stronglyConnComp graph
linBinds nonrec = return [nonrec]

instance Linearize (TypDefn 'Untyped) where
        linearize (DatDefn id qns ctors) = do
                _ <- linearize $ concatMap (fst . splitConstrTy . unLoc . snd) ctors
                return $ DatDefn id qns ctors

linDatDefns :: XTypDefns 'Untyped -> Writer [Ident] [XTypDefns 'Untyped]
linDatDefns (Mutrec tdefs) = do
        graph <- forM tdefs $ \tdef@(L _ (DatDefn id _ _ _)) -> do
                (tdef', chs) <- listen $ linearize tdef
                return (tdef', stamp id, map stamp chs)
        return $ stronglyConnComp graph
linDatDefns nonrec = return [nonrec]

linearizeTop :: Prog 'Untyped -> Prog 'Untyped
linearizeTop = concatMap $ \case
        ValDefn binds -> fst $ runWriter $ map ValDefn <$> linBinds binds
        TypDefn tdefs -> fst $ runWriter $ map TypDefn <$> linDatDefns tdefs