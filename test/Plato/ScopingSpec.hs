module Plato.ScopingSpec where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text qualified as T
import Test.Hspec

import Plato.Parsing
import Plato.Parsing.Parser
import Plato.Scoping

spec :: Spec
spec = do
        describe "Scope checking of expressions" $ do
                it "" $ do
                        head [23 ..] `shouldBe` (23 :: Int)

checkExpr :: (MonadIO m, MonadThrow m) => T.Text -> m ()
checkExpr inp = do
        exp <- parsePartial inp exprParser
        _exp' <- runReaderT (mapM scoping exp) initScope
        return ()