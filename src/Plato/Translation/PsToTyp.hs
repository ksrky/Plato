{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Translation.PsToTyp where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T
import Plato.Typing.Env
import Plato.Typing.Error
import Plato.Typing.Rename

import Control.Monad.Writer
import qualified Data.Text as T

transExpr :: RenameState -> Located P.Expr -> TypThrow (Located T.Expr)
transExpr st = traexpr
    where
        traexpr :: Located P.Expr -> TypThrow (Located T.Expr)
        traexpr (L sp (P.VarExpr x)) = return $ mkValue (names st) x
        traexpr (L sp (P.AppExpr e1 e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ L sp (T.AppExpr e1' e2')
        traexpr (L sp (P.OpExpr e1 op e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                let v = mkValue (names st) op
                return $ L sp (T.AppExpr (L sp $ T.AppExpr v e1') e2')
        traexpr (L sp (P.LamExpr xs e1)) = do
                e1' <- traexpr e1
                return $ foldr (\x e -> cLL x e $ T.AbsExpr x Nothing e) e1' xs
        traexpr (L sp (P.LetExpr ds e)) = do
                (d, names') <- transDecls st ds
                e' <- transExpr st{names = names'} e
                return $ L sp (T.LetExpr (noLoc d) e')
        traexpr (L sp (P.CaseExpr e alts)) = undefined
        traexpr (L _ (P.Factor _)) = unreachable "fixity resolution failed"

transType :: Located P.Type -> TypThrow (Located T.Type)
transType (L sp (P.VarType x)) = return $ L sp (T.VarType x)
transType (L sp (P.AppType ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ L sp (T.AppType ty1' ty2')
transType (L sp (P.ArrType ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ L sp (T.ArrType ty1' ty2')
transType (L sp (P.AllType xs ty1)) = do
        ty1' <- transType ty1
        return $ foldr (\x ty -> cLL x ty $ T.AllType x Nothing ty) ty1' xs

transDecls :: RenameState -> [Located P.Decl] -> TypThrow (T.Decl, Names)
transDecls st decs = do
        ns <- getFuncNames decs
        fieldtys <- execWriterT $
                forM decs $ \case
                        (L sp (P.FuncTyDecl x ty))
                                | unLoc x `elem` ns -> do
                                        ty' <- lift $ transType ty
                                        tell [(x, ty')]
                                | level st > 0 -> lift $ throwTypError sp "Variables should be declared in the top level"
                        _ -> return ()

        let (record, st') = undefined
            names' = names st ++ zip ns (repeat record)
        fields <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncDecl x xs e) | unLoc x `notElem` ns -> do
                                lift $ throwTypError sp "lacks type signature"
                        L sp (P.FuncDecl x xs e) -> do
                                e' <- lift $ transExpr st'{names = names'} (L sp $ P.LamExpr xs e)
                                tell [(x, e')]
                        _ -> return ()
        return (T.FuncDecl record (noLoc $ T.RecordExpr fields) (noLoc $ T.RecordType fieldtys), names')

transFuncTyDecls :: [Located P.Decl] -> TypThrow [Located T.Decl]
transFuncTyDecls decs = do
        xs <- getFuncNames decs
        execWriterT $
                forM decs $ \case
                        (L sp (P.FuncTyDecl x ty)) | unLoc x `notElem` xs -> do
                                ty' <- lift $ transType ty
                                tell [L sp $ T.VarDecl x ty']
                        _ -> return ()

getFuncNames :: [Located P.Decl] -> TypThrow [Name]
getFuncNames decs = execWriterT $
        forM decs $ \case
                (unLoc -> P.FuncDecl x _ _) -> tell [unLoc x]
                _ -> return ()

transTopDecl :: Located P.TopDecl -> WriterT ([Located T.Decl], [Located T.Decl]) TypThrow ()
transTopDecl (L sp (P.DataDecl name params fields)) = do
        fields' <- lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (l, tys')
        let fieldty = cLLn (fst $ head fields) (snd $ last fields) $ T.SumType fields'
        tell ([L sp (T.TypeDecl name (foldr (\x ty -> cLL x ty $ T.AbsType x Nothing ty) fieldty params))], [])
        forM_ fields' $ \(l, field) -> do
                let sumty = foldl (\ty1 ty2 -> cLL ty1 ty2 $ T.AppType ty1 ty2) (cL name $ T.VarType name) (map (\x -> cL x $ T.VarType x) params)
                    bodyty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrType ty1 ty2) bodyty field
                    ty = foldr (\x ty -> cLL x ty $ T.AllType x Nothing ty) bodyty params
                    tyargs = map (noLoc . str2tyvarName . show) [1 .. length params]
                    args = map (noLoc . varName . T.pack . show) [length params + 1 .. length params + length field]
                    tag = cLLn l args $ T.TagExpr l (map (\x -> cL x $ T.VarExpr x) args) Nothing
                    exp = foldr (\x e -> cLL x e $ T.AbsExpr x Nothing e) tag (tyargs ++ args)
                tell ([], [L sp $ T.FuncDecl l exp ty])
transTopDecl (L sp (P.TypeDecl name params ty1)) = do
        ty1' <- lift $ transType ty1
        tell ([L sp (T.TypeDecl name (foldr (\x ty -> cLL x ty $ T.AbsType x Nothing ty) ty1' params))], [])
transTopDecl _ = return ()

getDecls :: [Located P.TopDecl] -> [Located P.Decl]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

getEntry :: [Located P.Decl] -> (Located P.Decl, Located P.Decl)
getEntry decs = execWriter $
        forM decs $ \d -> case d of
                (unLoc -> P.FuncDecl f _ _) | unLoc f == entryPoint -> tell (d, mempty)
                (unLoc -> P.FuncTyDecl f _) | unLoc f == entryPoint -> tell (mempty, d)
                _ -> return ()

transEntry :: RenameState -> [Located P.Decl] -> Located T.Decl -> TypThrow (Located T.Expr, Located T.Type)
transEntry st decs bind = do
        (body, bodyty) <- execWriterT $
                forM decs $ \case
                        (unLoc -> P.FuncDecl f xs e) | unLoc f == entryPoint -> do
                                e' <- lift $ transExpr st (cLnL xs e $ P.LamExpr xs e)
                                tell (e', mempty)
                        (unLoc -> P.FuncTyDecl f ty) | unLoc f == entryPoint -> do
                                ty' <- lift $ transType ty
                                tell (mempty, ty')
                        _ -> return ()
        if body == mempty
                then
                        if bodyty == mempty
                                then return (noLoc $ T.LetExpr bind (noLoc $ T.RecordExpr []), noLoc $ T.RecordType [])
                                else throwTypError (getSpan bodyty) "lacks a binding"
                else
                        if bodyty == mempty
                                then throwTypError (getSpan body) "lacks a type signature"
                                else return (noLoc $ T.LetExpr bind body, bodyty)

abs2typ :: RenameState -> P.Program -> TypThrow T.Decls
abs2typ st (P.Program _ ids tds) = do
        let modns = map (\(L _ (P.ImpDecl mn)) -> mn) ids
        (tydecls, condecls) <- execWriterT $ mapM_ transTopDecl tds
        vardecls <- transFuncTyDecls (getDecls tds)
        (bind, names') <- transDecls st (getDecls tds)
        main <- transEntry st{names = names'} (getDecls tds) (noLoc bind)
        return $ T.Decls modns (tydecls ++ condecls ++ vardecls) main