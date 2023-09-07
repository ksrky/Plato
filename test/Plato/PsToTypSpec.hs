{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTypSpec where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Context
import Plato.Driver.Monad
import Plato.Parsing
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Syntax.Typing

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "lambda abstraction" $ do
                        test_scexpr "\\x -> x"
                                >>= ( `shouldSatisfy`
                                        (\case AbsE id1 Nothing (L _ (VarE id1')) -> check [(id1, id1')]; _ -> False)
                                    )
                it "Unbound variable" $ do
                        test_scexpr "\\x -> y" `shouldThrow` anyException
                it "parameter name conflict" $ do
                        test_scexpr "\\x x -> x" `shouldThrow` anyException
                it "let binding" $ do
                        test_scexpr "let {x : ty; x = exp} in x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                LetE [Bind (id1, _) _] (L _ (VarE id1')) -> check [(id1, id1')]
                                                _ -> False
                                        )
                                    )
                it "pattern abstraction" $ do
                        test_scexpr "\\(Con x) -> x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                AbsE id1 Nothing (L _ (CaseE (L _ (VarE id1')) [(L _ (ConP _ [L _ (VarP id2)]), L _ (VarE id2'))])) ->
                                                        check [(id1, id1'), (id2, id2')]
                                                _ -> False
                                        )
                                    )
        describe "Scope checking of declarations" $ do
                it "function clause" $ do
                        test_defns "id : {a} a -> a; id x = x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                [_, ValDefn [Bind _ [([L _ (VarP x)], L _ (VarE x'))]]] -> check [(x, x')]
                                                _ -> False
                                        )
                                    )
        describe "Scope checking of a file" $ do
                it "04.pla" $ do
                        test_scfile "04.pla"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                [_, ValDefn [Bind _ [([L _ (VarP f), L _ (VarP x)], L _ (AppE (L _ (VarE f')) (L _ (VarE x'))))]]] ->
                                                        check [(f, f'), (x, x')]
                                                _ -> False
                                        )
                                    )
        describe "Test psToTyp" $ do
                it "10.pla" $ do
                        test_file "10.pla"
                                `shouldReturn` [ "data List (a : $45) where {Nil : List a; :: : a -> List a -> List a}"
                                               , "reverse : {a : $46} List a -> List a \
                                                 \where {l -> let {rev : {a : $47} List a -> List a -> List a \
                                                 \where {Nil a -> a; (:: x xs) a -> rev xs (:: x a)}} in rev l Nil}"
                                               ]
                it "15.pla" $ do
                        test_file "15.pla"
                                `shouldReturn` [ "data ChurchNum where {ChurchNum : ({a : $45} (a -> a) -> a -> a) -> ChurchNum}"
                                               , "runNum : ChurchNum -> {a : $46} (a -> a) -> a -> a where {(ChurchNum xs) -> xs}; \
                                                 \zero : ChurchNum where {-> ChurchNum (\\ s . \\ z . z)}; \
                                                 \succ : ChurchNum -> ChurchNum where {n -> ChurchNum (\\ s . \\ z . s (runNum n s z))}; \
                                                 \two : ChurchNum where {-> succ (succ zero)}"
                                               ]

testScope :: MonadIO m => IORef Uniq -> m Scope
testScope ref = do
        u1 <- pickUniq ref
        u2 <- pickUniq ref
        u3 <- pickUniq ref
        let ids =
                [ Ident{nameIdent = varName "exp", spanIdent = NoSpan, stamp = u1}
                , Ident{nameIdent = conName "Con", spanIdent = NoSpan, stamp = u2}
                , Ident{nameIdent = tyvarName "ty", spanIdent = NoSpan, stamp = u3}
                ]
        return $ extendScope ids mempty

test_scexpr :: (MonadIO m, MonadCatch m) => T.Text -> m (Expr 'Untyped)
test_scexpr inp = do
        ctx <- initContext
        exp <- runReaderT (parseExpr inp) ctx
        exp' <- runReaderT (opParse exp) mempty
        sc <- testScope uref
        runReaderT (elabExpr sc (unLoc exp')) ctx

test_defns :: (MonadIO m, MonadCatch m) => T.Text -> m [Defn 'Untyped]
test_defns inp = do
        ctx <- initContext
        decs <- runReaderT (parseDecls inp) ctx
        sc <- testScope uref
        runReaderT (elabTopDecls sc decs) ctx

test_scfile :: (MonadIO m, MonadCatch m) => String -> m (Prog 'Untyped)
test_scfile fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        psToTyp pssyn'
                )
                =<< initSession

test_file :: (MonadIO m, MonadCatch m) => String -> m [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        typsyn <- psToTyp pssyn'
                        return $ map (show . pretty) typsyn
                )
                =<< initSession

check :: [(Ident, Ident)] -> Bool
check = all (\(id1, id2) -> stamp id1 == stamp id2)