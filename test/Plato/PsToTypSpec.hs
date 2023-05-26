{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTypSpec where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Hspec

import Data.IORef
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Syntax.Typing hiding (Spec)

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "lambda abstraction" $ do
                        scopingExpr "\\x -> x"
                                >>= ( `shouldSatisfy`
                                        (\case AbsE id1 (L _ (VarE id1')) -> check [(id1, id1')]; _ -> False)
                                    )
                it "Unbound variable" $ do
                        scopingExpr "\\x -> y" `shouldThrow` anyException
                it "parameter name conflict" $ do
                        scopingExpr "\\x x -> x" `shouldThrow` anyException
                it "let binding" $ do
                        scopingExpr "let {x : ty; x = exp} in x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                LetE [(id1', _)] [(id1, _)] (L _ (VarE id1'')) -> check [(id1, id1'), (id1, id1'')]
                                                _ -> False
                                        )
                                    )
                it "pattern abstraction" $ do
                        scopingExpr "\\(Con x) -> x"
                                >>= ( `shouldSatisfy`
                                        ( \case
                                                AbsE id1 (L _ (CaseE (L _ (VarE id1')) [(L _ (ConP _ [L _ (VarP id2)]), L _ (VarE id2'))])) ->
                                                        check [(id1, id1'), (id2, id2')]
                                                _ -> False
                                        )
                                    )

defScope :: Scope
defScope =
        M.fromList
                [ (varName "exp", Ident{nameIdent = varName "exp", spanIdent = NoSpan, stamp = uniqZero})
                , (conName "Con", Ident{nameIdent = conName "Con", spanIdent = NoSpan, stamp = uniqZero})
                , (tyvarName "ty", Ident{nameIdent = tyvarName "ty", spanIdent = NoSpan, stamp = uniqZero})
                ]

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

scopingExpr :: (MonadIO m, MonadThrow m) => T.Text -> m (Expr 'TcUndone)
scopingExpr inp = do
        exp <- parsePartial inp exprParser
        uniq <- initUniq
        runReaderT (elabExpr (unLoc exp)) (Context uniq defScope)

check :: [(Ident, Ident)] -> Bool
check = all (\(id1, id2) -> stamp id1 == stamp id2)