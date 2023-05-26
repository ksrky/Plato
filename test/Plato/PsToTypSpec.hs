{-# LANGUAGE LambdaCase #-}

module Plato.ScopingSpec where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Hspec

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.Scoping
import Plato.Syntax.Parsing

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "lambda abstraction" $ do
                        scopingExpr "\\x -> x"
                                >>= ( `shouldSatisfy`
                                        (\case LamE [L _ (VarP id1)] (L _ (VarE id2)) -> stamp id1 == stamp id2; _ -> False)
                                    )
                it "Unbound variable" $ do
                        scopingExpr "\\x -> y" `shouldThrow` anyException
                it "parameter name conflict" $ do
                        scopingExpr "\\x x -> x" `shouldThrow` anyException
                it "let binding" $ do
                        scopingExpr "let {x : ty; x = exp} in x"
                                >>= ( `shouldSatisfy`
                                        (\case LetE [_, L _ (FunBind id1 _)] (L _ (VarE id2)) -> stamp id1 == stamp id2; _ -> False)
                                    )
                it "pattern abstraction" $ do
                        scopingExpr "\\(Con x) -> x"
                                >>= ( `shouldSatisfy`
                                        (\case LamE [L _ (ConP _ [L _ (VarP id1)])] (L _ (VarE id2)) -> stamp id1 == stamp id2; _ -> False)
                                    )

defScope :: Scope
defScope =
        M.fromList
                [ (varName "exp", Ident{nameIdent = varName "exp", spanIdent = NoSpan, stamp = uniqZero})
                , (conName "Con", Ident{nameIdent = conName "Con", spanIdent = NoSpan, stamp = uniqZero})
                , (tyvarName "ty", Ident{nameIdent = tyvarName "ty", spanIdent = NoSpan, stamp = uniqZero})
                ]

scopingExpr :: (MonadIO m, MonadThrow m) => T.Text -> m Expr
scopingExpr inp = do
        exp <- parsePartial inp exprParser
        exp' <- runReaderT (mapM scoping exp) defScope
        return $ unLoc exp'