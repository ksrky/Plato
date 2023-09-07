{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Linearize where

import Control.Monad.Writer
import Data.Graph

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing

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
                binds' <- linBinds binds
                exp' <- linearize exp
                return $ LetE binds' exp'
        linearize (CaseE exp alts) = do
                exp' <- linearize exp
                alts' <- mapM (\(p, e) -> (p,) <$> linearize e) alts
                return $ CaseE exp' alts'
        linearize (AnnE exp ty) = AnnE <$> linearize exp <*> pure ty

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
        linearize (Bind idty clses) = Bind idty <$> mapM (\(ps, e) -> (ps,) <$> linearize e) clses

linBinds :: [Bind 'Untyped] -> Writer [Ident] [Bind 'Untyped]
linBinds binds = do
        graph <- forM binds $ \bnd@(Bind (par, _) _) -> do
                let (bnd', chs) = runWriter $ linearize bnd
                return (bnd', stamp par, map stamp chs)
        let sccs = stronglyConnComp graph
        return $ flattenSCCs sccs

instance Linearize (TypDefn 'Untyped) where
        linearize (DatDefn id qns ctors) = DatDefn id qns <$> mapM (\(id, ty) -> (id,) <$> linearize ty) ctors

linDatDefns :: [TypDefn 'Untyped] -> Writer [Ident] [TypDefn 'Untyped]
linDatDefns tdefs = do
        graph <- forM tdefs $ \tdef@(DatDefn id _ _) -> do
                let (tdef', chs) = runWriter $ linearize tdef
                return (tdef', stamp id, map stamp chs)
        let sccs = stronglyConnComp graph
        return $ flattenSCCs sccs

instance Linearize (Defn 'Untyped) where
        linearize (ValDefn binds) = ValDefn <$> linBinds binds
        linearize (TypDefn tdefs) = TypDefn <$> linDatDefns tdefs

linearizeTop :: Prog 'Untyped -> Prog 'Untyped
linearizeTop defs = fst $ runWriter (linearize defs)