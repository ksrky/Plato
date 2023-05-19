{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.PsToTyp where

import Control.Exception.Safe
import Control.Monad.Reader

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Monad

elabExpr :: P.Expr -> T.Expr
elabExpr (P.VarE id) = T.VarE id
elabExpr (P.AppE fun arg) = T.AppE (elabExpr <$> fun) (elabExpr <$> arg)
elabExpr (P.LamE pats body) = undefined
-- unLoc $ foldr (\p e -> sL p e $ T.PAbsE p Nothing e) (elabExpr <$> body) (map (elabPat <$>) pats)
elabExpr (P.LetE decs body) = do
        undefined

elabPat :: P.Pat -> T.Pat
elabPat (P.ConP con pats) = T.ConP con (map (elabPat <$>) pats)
elabPat (P.VarP var) = T.VarP var
elabPat P.WildP = T.WildP

elabType :: P.Type -> T.Type
elabType (P.VarT path) = T.VarT (T.BoundTv path)
elabType (P.ConT con) = T.ConT con
elabType (P.ArrT arg res) = T.ArrT (elabType <$> arg) (elabType <$> res)
elabType (P.AllT qnts body) = T.AllT (map (\id -> (T.BoundTv id, Nothing)) qnts) (elabType <$> body)
elabType (P.AppT fun arg) = T.AppT (elabType <$> fun) (elabType <$> arg)

elabFunDecls :: [P.FunDecl] -> ([(Ident, T.LExpr)], [(Ident, T.Type)])
elabFunDecls _ = undefined

elabFunDecl :: P.FunDecl -> T.Decl
elabFunDecl (P.FunSpec id ty) = do
        let ty' = elabType <$> ty
        T.SpecDecl (T.ValSpec id ty')
elabFunDecl (P.FunBind id [] exp) = do
        let exp' = elabExpr <$> exp
        T.BindDecl (T.ValBind id Nothing exp')
elabFunDecl (P.FunBind id _ exp) = undefined

elabDecl ::
        (MonadReader env m, HasUniq env, MonadIO m) =>
        P.Decl ->
        m [T.Decl]
elabDecl (P.DataD id params constrs) = undefined {-do
                                                 let quantify :: [Ident] -> T.LType -> T.LType
                                                     quantify params ty = ty
                                                 let constrs' = map (\(con, ty) -> (con, quantify params $ elabType <$> ty)) constrs
                                                 let condecs = map (\(id, ty) -> T.SpecDecl (T.ValSpec id (unLoc ty))) constrs'
                                                 kv <- newKnVar
                                                 return $ T.SpecDecl (T.TypSpec id kv) : condecs-}
elabDecl (P.FuncD (P.FunSpec id sig)) = do
        let sig' = elabType <$> sig
        return [T.SpecDecl (T.ValSpec id sig')]
elabDecl (P.FuncD (P.FunBind id [] exp)) = do
        let exp' = elabExpr <$> exp
        return [T.BindDecl (T.ValBind id Nothing exp')]
elabDecl (P.FuncD fundec) = return [elabFunDecl fundec]

elabTopDecl :: P.TopDecl -> T.Decl
elabTopDecl (P.Decl dec) = undefined
elabTopDecl (P.Eval exp) = undefined

ps2typ :: (PlatoMonad m, MonadThrow m) => P.Program -> m T.Program
ps2typ = undefined