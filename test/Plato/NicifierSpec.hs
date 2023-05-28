module Plato.NicifierSpec where

-- import Plato.Parsing.OpParser

import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Nicifier.OpParser
import Plato.Nicifier.OpParser.Fixity
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

spec :: Spec
spec = do
        describe "Fixity resolution" $ do
                it "x + y" $ do
                        test "x + y" `shouldReturn` "(x + y)"
                it "x + y * z" $ do
                        test "x + y * z" `shouldReturn` "(x + (y * z))"
                it "x ++ y ++ z" $ do
                        test "x ++ y ++ z" `shouldReturn` "(x ++ (y ++ z))"
                it "x > y > z" $ do
                        test "x > y > z" `shouldThrow` anyException
        describe "File test" $ do
                it "test05.plt" $ do
                        test_file "test05.plt"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool; not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "test07.plt" $ do
                        test_file "test07.plt"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool; not where {True -> False}; not where {False -> True}"
                                               ]

fixityEnv :: FixityEnv
fixityEnv =
        M.fromList
                [ (varName "*", Fixity 7 Leftfix)
                , (varName "+", Fixity 6 Leftfix)
                , (varName "-", Fixity 6 Leftfix)
                , (varName "++", Fixity 5 Rightfix)
                , (varName ">", Fixity 4 Nonfix)
                ]

test :: T.Text -> IO String
test inp = do
        exp <- runReaderT (parsePartial inp exprParser) =<< initUniq
        exp' <- runReaderT (opParse exp) fixityEnv
        return $ show $ pretty exp'

test_file :: String -> IO [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        return $ map (show . pretty) pssyn'
                )
                =<< initSession