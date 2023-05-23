module Plato.Test.OpParserSpec where

-- import Plato.Parsing.OpParser

import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Name
import Plato.Parsing
import Plato.Parsing.OpParser
import Plato.Parsing.OpParser.Fixity
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

spec :: Spec
spec = do
        describe "Fixity resolution" $ do
                it "x" $ do
                        opParse "x" `shouldReturn` "x"
                it "x + y" $ do
                        opParse "x + y" `shouldReturn` "(x + y)"
                it "x + y * z" $ do
                        opParse "x + y * z" `shouldReturn` "((x + y) * z)"
                it "x ++ y ++ z" $ do
                        opParse "x ++ y ++ z" `shouldReturn` "(x ++ (y ++ z))"
                it "x > y > z" $ do
                        opParse "x > y > z" `shouldThrow` anyException

fixityEnv :: FixityEnv
fixityEnv =
        M.fromList
                [ (varName "+", Fixity 6 Leftfix)
                , (varName "-", Fixity 6 Leftfix)
                , (varName "++", Fixity 5 Rightfix)
                , (varName ">", Fixity 4 Nonfix)
                ]

opParse :: T.Text -> IO String
opParse inp = do
        exp <- parsePartial inp exprParser
        exp' <- runReaderT (opParser exp) fixityEnv
        return $ show $ pretty exp'