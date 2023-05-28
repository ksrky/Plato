{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTypSpec where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Hspec

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Syntax.Typing hiding (Spec)

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "lambda abstraction" $ do
                        test_expr "\\x -> x"
                                >>= ( `shouldSatisfy`
                                        (\case AbsE id1 (L _ (VarE id1')) -> check [(id1, id1')]; _ -> False)
                                    )
                it "Unbound variable" $ do
                        test_expr "\\x -> y" `shouldThrow` anyException
                it "parameter name conflict" $ do
                        test_expr "\\x x -> x" `shouldThrow` anyException
                it "let binding" $ do
                        test_expr "let {x : ty; x = exp} in x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                LetE [(id1', _)] [(id1, _)] (L _ (VarE id1'')) -> check [(id1, id1'), (id1, id1'')]
                                                _ -> False
                                        )
                                    )
                it "pattern abstraction" $ do
                        test_expr "\\(Con x) -> x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                AbsE id1 (L _ (CaseE (L _ (VarE id1')) [(L _ (ConP _ [L _ (VarP id2)]), L _ (VarE id2'))])) ->
                                                        check [(id1, id1'), (id2, id2')]
                                                _ -> False
                                        )
                                    )
        describe "Scope checking of declarations" $ do
                it "function clause" $ do
                        test_decls "id : {a} a -> a; id x = x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                [_, BindDecl (FunBind _ [([L _ (VarP x)], L _ (VarE x'))])] -> check [(x, x')]
                                                _ -> False
                                        )
                                    )
        describe "Scope checking of aa file" $ do
                it "test04.plt" $ do
                        test_file "test04.plt"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                [_, BindDecl (FunBind _ [([L _ (VarP f), L _ (VarP x)], L _ (AppE (L _ (VarE f')) (L _ (VarE x'))))])] ->
                                                        check [(f, f'), (x, x')]
                                                _ -> False
                                        )
                                    )

defScope :: MonadIO m => IORef Uniq -> m Scope
defScope ref = do
        u1 <- pickUniq ref
        u2 <- pickUniq ref
        u3 <- pickUniq ref
        return $
                M.fromList
                        [ (varName "exp", Ident{nameIdent = varName "exp", spanIdent = NoSpan, stamp = u1})
                        , (conName "Con", Ident{nameIdent = conName "Con", spanIdent = NoSpan, stamp = u2})
                        , (tyvarName "ty", Ident{nameIdent = tyvarName "ty", spanIdent = NoSpan, stamp = u3})
                        ]

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

test_expr :: (MonadIO m, MonadThrow m) => T.Text -> m (Expr 'TcUndone)
test_expr inp = do
        uniq <- initUniq
        exp <- runReaderT (parsePartial inp exprParser) uniq
        sc <- defScope uniq
        runReaderT (elabExpr (unLoc exp)) (Context uniq sc)

test_decls :: (MonadIO m, MonadThrow m) => T.Text -> m [Decl 'TcUndone]
test_decls inp = do
        uniq <- initUniq
        decs <- runReaderT (parsePartial inp declsParser) uniq
        sc <- defScope uniq
        runReaderT (elabDecls $ map unLoc decs) (Context uniq sc)

test_file :: (MonadIO m, MonadThrow m) => String -> m (Program 'TcUndone)
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        ps2typ pssyn'
                )
                =<< initSession

check :: [(Ident, Ident)] -> Bool
check = all (\(id1, id2) -> stamp id1 == stamp id2)