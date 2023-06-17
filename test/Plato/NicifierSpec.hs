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
                                               , "not : Bool -> Bool"
                                               , "not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "test07.plt" $ do
                        test_file "test07.plt"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {True -> False}"
                                               , "not where {False -> True}"
                                               ]
                it "test14.plt" $ do
                        test_file "test14.plt"
                                `shouldReturn` [ "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "data T where {T1 : T; T2 : T; T3 : T}"
                                               , "f : List T"
                                               , "f where {-> (T1 :: (T2 :: (T3 :: Nil)))}"
                                               ]
                it "test16.plt" $ do
                        test_file "test16.plt"
                                `shouldReturn` [ "data :,: a b where {:,: : a -> b -> a :,: b}"
                                               , "fst : {a b} a :,: b -> a"
                                               , "fst where {(:,: x _) -> x}"
                                               , "snd : {a b} a :,: b -> b"
                                               , "snd where {(:,: _ y) -> y}"
                                               , "curry : {a b c} (a :,: b -> c) -> a -> b -> c"
                                               , "curry where {f x y -> f ((x :,: y))}"
                                               , "uncurry : {a b c} (a -> b -> c) -> a :,: b -> c"
                                               , "uncurry where {f (:,: x y) -> f x y}"
                                               , "swap : {a b} a :,: b -> b :,: a"
                                               , "swap where {(:,: x y) -> (y :,: x)}"
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