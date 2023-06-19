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
import Prettyprinter
import Test.Hspec

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Nicifier.OpParser
import Plato.Nicifier.OpParser.Fixity
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Syntax.Typing hiding (Spec)

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "lambda abstraction" $ do
                        test_scexpr "\\x -> x"
                                >>= ( `shouldSatisfy`
                                        (\case AbsE id1 (L _ (VarE id1')) -> check [(id1, id1')]; _ -> False)
                                    )
                it "Unbound variable" $ do
                        test_scexpr "\\x -> y" `shouldThrow` anyException
                it "parameter name conflict" $ do
                        test_scexpr "\\x x -> x" `shouldThrow` anyException
                it "let binding" $ do
                        test_scexpr "let {x : ty; x = exp} in x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                LetE [(id1', _)] [(id1, _)] (L _ (VarE id1'')) -> check [(id1, id1'), (id1, id1'')]
                                                _ -> False
                                        )
                                    )
                it "pattern abstraction" $ do
                        test_scexpr "\\(Con x) -> x"
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
                                                [_, DefnDecl (FunDefn _ [([L _ (VarP x)], L _ (VarE x'))])] -> check [(x, x')]
                                                _ -> False
                                        )
                                    )
        describe "Scope checking of a file" $ do
                it "test04.plt" $ do
                        test_scfile "test04.plt"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                [_, DefnDecl (FunDefn _ [([L _ (VarP f), L _ (VarP x)], L _ (AppE (L _ (VarE f')) (L _ (VarE x'))))])] ->
                                                        check [(f, f'), (x, x')]
                                                _ -> False
                                        )
                                    )
        describe "Test ps2typ" $ do
                it "test10.plt" $ do
                        test_file "test10.plt"
                                `shouldReturn` [ "List : $48"
                                               , "data List (a:$45) where {Nil : List a; :: : a -> List a -> List a}"
                                               , "reverse : {a:$46} List a -> List a"
                                               , "reverse where {l -> let {rev = {a:$47} List a -> List a -> List a; rev where {Nil a -> a; (:: x xs) a -> rev xs (:: x a)}} in rev l Nil}"
                                               ]
                it "test15.plt" $ do
                        test_file "test15.plt"
                                `shouldReturn` [ "ChurchNum : $47"
                                               , "data ChurchNum where {ChurchNum : ({a:$45} (a -> a) -> a -> a) -> ChurchNum}"
                                               , "runNum : ChurchNum -> ({a:$46} (a -> a) -> a -> a)"
                                               , "zero : ChurchNum"
                                               , "succ : ChurchNum -> ChurchNum"
                                               , "two : ChurchNum"
                                               , "runNum where {(ChurchNum xs) -> xs}"
                                               , "zero where {-> ChurchNum (\\s. \\z. z)}"
                                               , "succ where {n -> ChurchNum (\\s. \\z. s (runNum n s z))}"
                                               , "two where {-> succ (succ zero)}"
                                               ]

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
        setUniq uniq ref = setUniq uniq (ctx_uniq ref)

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

test_scexpr :: (MonadIO m, MonadThrow m) => T.Text -> m (Expr 'TcUndone)
test_scexpr inp = do
        uniq <- initUniq
        exp <- runReaderT (parsePartial inp exprParser) uniq
        exp' <- runReaderT (opParse exp) initFixityEnv
        sc <- defScope uniq
        runReaderT (elabExpr (unLoc exp')) (Context uniq sc)

test_decls :: (MonadIO m, MonadThrow m) => T.Text -> m [Decl 'TcUndone]
test_decls inp = do
        uniq <- initUniq
        decs <- runReaderT (parsePartial inp declsParser) uniq
        sc <- defScope uniq
        runReaderT (elabDecls decs) (Context uniq sc)

test_scfile :: (MonadIO m, MonadThrow m) => String -> m (Program 'TcUndone)
test_scfile fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        ps2typ pssyn'
                )
                =<< initSession

test_file :: (MonadIO m, MonadThrow m) => String -> m [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        typsyn <- ps2typ pssyn'
                        return $ map (show . pretty) typsyn
                )
                =<< initSession

check :: [(Ident, Ident)] -> Bool
check = all (\(id1, id2) -> stamp id1 == stamp id2)